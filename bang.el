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

(define-multisession-variable bang--last-ids nil)
(defvar bang--db nil)

(defun bang--poll-blogs ()
  (let ((blogs bang-blogs)
	(ids (multisession-value bang--last-ids))
	(data nil)
	func)
    (setq func
	  (lambda ()
	    (let* ((blog (pop blogs))
		   (id (or (plist-get ids (intern blog)) 0))
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
		     (bang--update-data ids data))))))))
    (funcall func)))      

(defun bang--update-data (ids data)
  (cl-loop for (blog . elems) in data
	   do (cl-loop for elem across (gethash "data" elems)
		       for (id time click page referrer ip user-agent) =
		       (cl-coerce elem 'list)
		       unless (bang--bot-p user-agent)
		       do (bang--insert-data time click page referrer ip
					     user-agent)
		       finally
		       (setf (plist-get ids (intern blog))
			     (string-to-number id))))
  (setf (multisession-value bang--last-ids) ids))

(defun bang--bot-p (user-agent)
  (string-match-p "Googlebot\\|AhrefsBot\\|[Bb]ot/" user-agent))

(defun bang--initialize ()
  (unless bang--db
    (setq bang--db (sqlite-open
		    (expand-file-name "bang.sqlite" user-emacs-directory)))
    (sqlite-execute bang--db "create table if not exists stats (id integer primary key, time text, click text, page text, referrer text, ip text, user_agent text)")))

(defun bang--insert-data (time click page referrer ip user-agent)
  (sqlite-execute
   bang--db
   "insert into stats(time, click, page, referrer, ip, user_agent) values(?, ?, ?, ?, ?, ?)"
   (list time click page referrer ip user-agent)))

(provide 'bang)

;;; bang.el ends here
