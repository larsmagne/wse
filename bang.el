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
  (setq d data)
  (cl-loop for (blog . elems) in data
	   do (cl-loop for elem across (gethash "data" elems)
		       for (id time click page referrer ip user-agent) =
		       (cl-coerce elem 'list)
		       unless (bang--bot-p user-agent)
		       do
		       (bang--insert-data blog time click page referrer ip
					  user-agent page)
		       (bang--update-id blog id))))

(defun bang--update-id (blog id)
  (if (sqlite-select bang--db "select last_id from blogs where blog = ?"
		     (list blog))
      (sqlite-execute bang--db "update blogs set last_id = ? where blog = ?"
		      (list id blog))
    (sqlite-execute bang--db "insert into blogs(blog, last_id) values(?, ?)"
		    (list blog id))))

(defun bang--bot-p (user-agent)
  (string-match-p "Googlebot\\|AhrefsBot\\|[Bb]ot/" user-agent))

(defun bang--initialize ()
  (unless bang--db
    (setq bang--db (sqlite-open
		    (expand-file-name "bang.sqlite" user-emacs-directory)))
    (sqlite-execute bang--db "create table if not exists blogs (blog text primary key, last_id integer)")
    (sqlite-execute bang--db "create table if not exists country_counter (id integer)")
    (sqlite-execute bang--db "create table if not exists views (id integer primary key, blog text, date date, time datetime, page text, ip text, user_agent text, title text, country text)")
    (sqlite-execute bang--db "create table if not exists referrers (id integer primary key, blog text, time datetime, referrer text, page text)")
    (sqlite-execute bang--db "create table if not exists clicks (id integer primary key, blog text, time datetime, click text, page text)")
    (sqlite-execute bang--db "create table if not exists history (id integer primary key, blog text, date date, views integer, visitors integer, clicks integer, referrers integer)")))

(defun bang--host (url)
  (url-host (url-generic-parse-url url)))

(defun bang--url-p (string)
  (and (not (zerop (length string)))
       (string-match-p "\\`[a-z]+:" string)))

(defun bang--insert-data (blog time click page referrer ip user-agent title)
  (when (bang--url-p page)
    (if (bang--url-p click)
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
       "insert into views(blog, date, time, page, ip, user_agent, title, country) values(?, ?, ?, ?, ?, ?, ?, ?)"
       (list blog (substring time 0 10) time page ip user-agent title ""))
      ;; Check whether to register a referrer.
      (when (and (bang--url-p referrer)
		 (not (equal (bang--host referrer) blog)))
	(sqlite-execute
	 bang--db
	 "insert into referrers(blog, time, referrer, page) values(?, ?, ?, ?)"
	 (list blog time referrer page))))))

(define-derived-mode bang-mode special-mode "Bang"
  "Major mode for listing Wordpress statistics."
  (setq truncate-lines t))

(defvar-keymap bang-mode-map
  "RET" #'push-button
  "<mouse-2>" #'push-button
  "g" #'bang-revert)

(defun bang-revert ()
  "Update the current buffer."
  (interactive))

(defun bang ()
  "Display Wordpress statistics."
  (interactive)
  (pop-to-buffer "*Bang*")
  (bang-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :columns '((:name "Number Of Pages" :align 'right)
		(:name "Posts & Pages" :width 35)
		(:name "Number Of Referrers" :align 'right)
		(:name "Referrers" :width 35))
     :objects (bang--get-page-table-data)
     :getter
     (lambda (elem column vtable)
       (if (member (vtable-column vtable column)
		   '("Posts & Pages" "Referrers"))
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)
    (goto-char (point-max))
    (insert "\n")
    (make-vtable
     :columns '((:name "Number Of Clicks" :align 'right)
		(:name "Clicks" :width 35)
		(:name "Number Of Countries" :align 'right)
		(:name "Countries" :width 35))
     :objects (bang--get-click-table-data)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--possibly-buttonize (string)
  (if (bang--url-p string)
      (buttonize string (lambda (_)
			  (let ((browse-url-browser-function
				 browse-url-secondary-browser-function))
			    (browse-url string))))
    string))

(defun bang--get-page-table-data ()
  (let* ((time (bang--time (- (time-convert (current-time) 'integer)
			      (* 60 60 24))))
	 (pages
	  (sqlite-select
	   bang--db
	   "select count(page), title from views where time > ? group by page order by count(page) desc limit 10"
	   (list time)))
	 (referrers
	  (bang--transform-referrers
	   (sqlite-select
	    bang--db
	    "select count(referrer), referrer from referrers where time > ? group by referrer order by count(referrer) desc"
	    (list time)))))
    (cl-loop for i from 0 upto 9
	     collect
	     (append (or (elt pages i) (list nil nil))
		     (or (elt referrers i) (list nil nil))))))

(defun bang--get-click-table-data ()
  (let* ((time (bang--time (- (time-convert (current-time) 'integer)
			      (* 60 60 24))))
	 (clicks
	  (sqlite-select
	   bang--db
	   "select count(click), click from clicks where time > ? group by click order by count(click) desc limit 10"
	   (list time)))
	 (countries
	  (sqlite-select
	   bang--db
	   "select count(country), country from views where time > ? group by country order by count(country) desc limit 10"
	   (list time))))
    (cl-loop for i from 0 upto 9
	     collect
	     (append (or (elt clicks i) (list "" ""))
		     (or (elt countries i) (list "" ""))))))

(defun bang--transform-referrers (referrers)
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for (count url) in referrers
	     do (cl-incf (gethash (bang--transform-referrer url) table 0)
			 count))
    (let ((result nil))
      (maphash (lambda (referrer count)
		 (push (list count referrer) result))
	       table)
      (seq-take (nreverse (sort result #'car-less-than-car)) 10))))

(defun bang--transform-referrer (url)
  (cond
   ((string-match-p "[.]bing[.][a-z]+/\\'" url)
    "Bing")
   ((string-match-p "[.]google[.][a-z]+/\\'" url)
    "Google")
   ((string-match-p "[.]reddit[.]com/\\'" url)
    "Reddit")
   ((string-match-p "\\bduckduckgo[.]com/\\'" url)
    "DuckDuckGo")
   ((string-match-p "\\byandex[.]ru/\\'" url)
    "Yandex")
   ((equal (bang--host url) "t.co")
    "Twitter")
   (t
    url)))
   

(defun bang--time (time)
  (format-time-string "%Y-%m-%d %H:%M:%S" time))

(provide 'bang)

;;; bang.el ends here
