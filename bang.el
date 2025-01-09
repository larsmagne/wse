;;; bang.el --- Show Wordpress statistics -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; bang is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; bang is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;;; Code:

(require 'cl-lib)

(defvar bang-blogs nil
  "A list of blogs to collect statistics from.
This should be a list of names (like \"foo.org\" and not URLs.")

(defvar bang--db nil)

(defun bang--poll-blogs ()
  (let ((blogs bang-blogs)
	(data nil)
	func)
    (setq func
	  (lambda ()
	    (let* ((blog (pop blogs))
		   (id (or (sqlite-select bang--db "select last_id from blogs where blog = ?"
					  (list blog))
			   0))
		   (url-request-method "POST")
		   (url-request-extra-headers
		    '(("Content-Type" . "application/x-www-form-urlencoded")
		      ("Charset" . "UTF-8")))
		   (url-request-data
		    (mm-url-encode-www-form-urlencoded
		     `(("form_id" . ,(format "%d" id))
		       ("password" . ,(auth-info-password
				       (car
					(auth-source-search
					 :max 1
					 :user "bang"
					 :host blog
					 :require '(:user :secret)
					 :create t))))))))
	      (url-retrieve
	       (format "https://%s/wp-content/plugins/bang/data.php" blog)
	       (lambda (status)
		 (goto-char (point-min))
		 (unwind-protect
		     (and (search-forward "\n\n" nil t)
			  (not (plist-get status :error))
			  (push (cons blog (json-parse-buffer)) data))
		   (kill-buffer (current-buffer))
		   (if blogs
		       (funcall func)
		     (bang--update-data data))))))))
    (funcall func)))      

(defun bang--update-data (data)
  (cl-loop for (blog . elems) in data
	   do (cl-loop for elem across (gethash "data" elems)
		       for (id time click page referrer ip user-agent) =
		       (cl-coerce elem 'list)
		       unless (bang--bot-p user-agent)
		       do
		       (bang--insert-data blog time click page referrer ip
					  user-agent)
		       (bang--update-id blog id))))

(defun bang--update-id (blog id)
  (if (sqlite-select bang--db "select last_id from blogs where blog = ?"
		     (list blog))
      (sqlite-execute bang--db "update blogs set last_id = ? where blog = ?"
		      (list id blog))
    (sqlite-execute bang--db "insert into blogs(blog, id) values(?, ?)"
		    (list blog id))))

(defun bang--bot-p (user-agent)
  (string-match-p "Googlebot\\|AhrefsBot\\|[Bb]ot/" user-agent))

(defun bang--initialize ()
  (unless bang--db
    (setq bang--db (sqlite-open
		    (expand-file-name "bang.sqlite" user-emacs-directory)))
    (sqlite-execute bang--db "create table if not exists blogs (blog text primary key, last_id integer)")
    (sqlite-execute bang--db "create table if not exists country_counter (id integer)")
    (sqlite-execute bang--db "create table if not exists views (id integer primary key, blog text, date date, time datetime, page text, ip text, user_agent text, country text)")
    (sqlite-execute bang--db "create table if not exists referrers (id integer primary key, blog text, time datetime, referrer text, page text)")
    (sqlite-execute bang--db "create table if not exists clicks (id integer primary key, blog text, time datetime, click text, page text)")
    (sqlite-execute bang--db "create table if not exists history (id integer primary key, blog text, date date, views integer, visitors integer, clicks integer, referrers integer)")))

(defun bang--host (url)
  (url-domain (url-generic-parse-url url)))

(defun bang--insert-data (blog time click page referrer ip user-agent)
  (if (not (zerop (length click)))
      ;; Register a click if it's not going to the current blog, or
    ;; whether it's going to a media URL of some kind (image/mp4/etc).
    (when (or (not (equal (bang--host click) blog))
	      (string-match "/wp-contents/uploads/" click)
	      (string-match "[.]\\(mp4\\|png\\|jpg\\|jpeg\\|webp\\|webp\\)\\'"
			    click))
      (sqlite-execute
       bang--db
       "insert into clicks(blog, time, click, page) values(?, ?, ?, ?)"
       (list blog time click page)))
    ;; Insert into views.
    (sqlite-execute
     bang--db
     "insert into views(blog, date, time, click, page, referrer, ip, user_agent, country) values(?, ?, ?, ?, ?, ?)"
     (list blog (substring time 0 10) time click page
	   referrer ip user-agent ""))
    ;; Check whether to register a referrer.
    (when (and (not (zerop (length referrer)))
	       (not (equal (bang--host referrer) blog)))
      (sqlite-execute
       bang--db
       "insert into referrers(blog, time, referrer, page) values(?, ?, ?, ?)"
       (list blog time referrer page)))))

(provide 'bang)

;;; bang.el ends here
