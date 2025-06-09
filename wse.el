;;; wse.el --- Show Wordpress statistics -*- lexical-binding: t -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Lars Magne Ingebrigtsen <larsi@gnus.org>

;; wse is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; wse is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.

;;; Commentary:

;; To detect browser/os, wse uses a Perl library:
;; apt install libjson-perl libhttp-browserdetect-perl

;;; Code:

(require 'cl-lib)
(require 'eplot)
(require 'url-domsuf)
(require 'shr)

(defvar wse-font "sans-serif"
  "Font family to use in buffer and charts.")

(defface wse
  `((t :family ,wse-font))
  "The face to use in wse buffers.")

(defvar wse-blogs nil
  "A list of blogs to collect statistics from.
This should be a list of names (like \"foo.org\" and not URLs.")

(defvar wse-entries 12
  "The number of entries to display.")

;; Internal variables.
(defvar wse--db nil)
(defvar wse--filling-country nil)
(defvar wse--timer nil)

(defun wse ()
  "Display Wordpress statistics."
  (interactive)
  (switch-to-buffer "*Wordpress Statistics*")
  (wse--render))

(defun wse-update-automatically ()
  "Update *Wordpress Statistics* automatically periodically."
  (interactive)
  (when wse--timer
    (cancel-timer wse--timer))
  (setq wse--timer (run-at-time 60 (* 60 5) #'wse--update))
  (message "Updating statistics every five minutes"))

;; This is a separate function instead of a lambda so that it's easier
;; to find in `M-x list-timers'.
(defun wse--update ()
  (when-let ((idle (current-idle-time))
	     (buffer (get-buffer "*Wordpress Statistics*")))
    (when (> (time-convert (time-since idle) 'integer) 20)
      (with-current-buffer buffer
	(wse-revert t)))))

;; Helper functions.

(defun wse--bot-p (user-agent)
  (or
   (let ((case-fold-search t))
     (string-match-p "bot/\\|spider\\b\\|crawl" user-agent))
   (let ((case-fold-search nil))
     (string-match-p
      "Bot\\|meta-externalagent\\|HeadlessChrome\\|Google-Read-Aloud"
      user-agent))))

(defun wse--host (url)
  (url-host (url-generic-parse-url url)))

(defun wse--url-p (string)
  (and (and (stringp string))
       (not (zerop (length string)))
       (string-match-p "\\`[a-z]+:" string)))

(defun wse--media-p (click)
  (string-match "[.]\\(mp4\\|png\\|jpg\\|jpeg\\|webp\\|webp\\|gif\\)\\'" click))

(defun wse--countrify (code name)
  (if (= (length code) 2)
      ;; Convert the country code into a Unicode flag.
      (concat (string (+ #x1f1a5 (elt code 0)) (+ #x1f1a5 (elt code 1)))
	      " " name)
    name))

(defun wse--pretty-url (string)
  (replace-regexp-in-string "\\`[a-z]+://\\(www[.]\\)?" "" string))

(defun wse--possibly-buttonize (string)
  (if (wse--url-p string)
      (buttonize (wse--pretty-url string) #'wse--browse string string)
    string))

(defun wse--time (time)
  (format-time-string "%Y-%m-%d %H:%M:%S" time "Z"))

(defun wse--24h ()
  (wse--time (- (time-convert (current-time) 'integer)
		(* 60 60 24))))

(defun wse--1h ()
  (wse--time (- (time-convert (current-time) 'integer)
		(* 60 60 1))))

(defun wse--future ()
  "9999-12-12 23:59:00")

(defun wse--parse-time (time)
  "Decode TIME, which is GMT/Z/UTC."
  (encode-time (iso8601-parse (concat (string-replace " " "T" time) "Z"))))

(defun wse--browse (url)
  (let ((browse-url-browser-function
	 (if (and (wse--media-p url)
		  (not (string-match "[.]mp4\\'" url)))
	     browse-url-browser-function
	   browse-url-secondary-browser-function)))
    (browse-url url)))

(defun wse--get-domain (host)
  "Return the shortest domain that refers to an entity.
I.e., \"google.com\" or \"google.co.uk\"."
  (let* ((bits (reverse (split-string host "[.]")))
	 (domain (pop bits)))
    (cl-loop while (and bits
			(not (url-domsuf-cookie-allowed-p domain)))
	     do (setq domain (concat (pop bits) "." domain)))
    domain))

(defun wse-sel (statement &rest args)
  (sqlite-select wse--db statement args))

(defun wse-exec (statement &rest args)
  (sqlite-execute wse--db statement args))

(defun wse--in (list)
  (mapconcat (lambda (_) "?") list ","))

(defun wse--local-time (time)
  (format-time-string "%Y-%m-%d %H:%M:%S"
		      (wse--parse-time time)))

(defun wse--weekend-p (date)
  (memq
   (decoded-time-weekday
    (decode-time (encode-time (decoded-time-set-defaults
			       (iso8601-parse-date date)))))
   '(0 6)))

(defun wse--file (file)
  (expand-file-name file (file-name-directory (locate-library "wse"))))

(defun wse--adjust-title (title url)
  (cond
   ((string-match "/category/" url)
    (concat "Category: " title))
   ((string-match "/author/" url)
    (concat "Author: " title))
   (t
    title)))

;; Update data.

(defun wse--poll-blogs (&optional callback)
  (let ((blogs wse-blogs)
	(data nil)
	func)
    (setq func
	  (lambda ()
	    (let* ((blog (pop blogs))
		   (ids (or (car
			     (wse-sel "select last_id, last_comment_id from blogs where blog = ?"
				      blog))
			    '(0 0)))
		   (url-request-method "POST")
		   (url-request-extra-headers
		    '(("Content-Type" . "application/x-www-form-urlencoded")
		      ("Charset" . "UTF-8")))
		   (url-request-data
		    (mm-url-encode-www-form-urlencoded
		     `(("from_id" . ,(format "%d" (car ids)))
		       ("from_comment_id" . ,(format "%d" (or (cadr ids) 0)))
		       ("password" . ,(auth-info-password
				       (car
					(auth-source-search
					 :max 1
					 :user "wse"
					 :host blog
					 :require '(:user :secret)
					 :create t))))))))
	      (url-retrieve
	       (format "https://%s/wp-content/plugins/wse/data.php" blog)
	       (lambda (status)
		 (goto-char (point-min))
		 (unwind-protect
		     (and (search-forward "\n\n" nil t)
			  (not (plist-get status :error))
			  (push (cons blog (json-parse-buffer)) data))
		   (kill-buffer (current-buffer))
		   (if blogs
		       (funcall func)
		     (wse--update-data data callback))))
	       nil t))))
    (funcall func)))      

(defvar wse--rate-limit-table (make-hash-table :test #'equal))

(defun wse--rate-limit (time ip click page)
  (let* ((is-click (not (zerop (length click))))
	 (url (wse--clean-url (if is-click click page)))
	 (prev (gethash (list is-click ip url) wse--rate-limit-table)))
    (cond
     ((not prev)
      (setf (gethash (list is-click ip url) wse--rate-limit-table) time)
      nil)
     ;; If less than an hour, rate limit.
     ((< (- (time-convert (wse--parse-time time) 'integer)
	    (time-convert (wse--parse-time prev) 'integer))
	 (* 60 60))
      t)
     (t
      (setf (gethash (list is-click ip url) wse--rate-limit-table) time)
      nil))))

(defun wse--update-data (data &optional callback)
  (cl-loop for (blog . elems) in data
	   do (cl-loop for elem across (gethash "data" elems)
		       for (id time click page referrer ip user-agent title) =
		       (cl-coerce elem 'list)
		       when (and (not (zerop (length click)))
				 (not (wse--url-p click)))
		       ;; Expand relative URLs.
		       do (setq click (shr-expand-url click page))
		       when (not (stringp user-agent))
		       do (setq user-agent "")
		       ;; If we're running two updates at
		       ;; the same time, ignore second update.
		       when (> (string-to-number id)
			       (or (caar
				    (wse-sel
				     "select last_id from blogs where blog = ?"
				     blog))
				   -1))
		       do
		       (when (and (not (wse--bot-p user-agent))
				  (not (wse--rate-limit
					time ip click page)))
			 (wse--insert-data blog time
					   click page referrer ip
					   user-agent title))
		       (wse--update-id blog id))
	   do (wse--store-comments blog (gethash "comments" elems)))

  (wse--fill-browser)
  (unless wse--filling-country
    (wse--fill-country))
  (wse--possibly-summarize-history)
  (when callback
    (funcall callback)))

(defun wse--update-id (blog id)
  (if (wse-sel "select last_id from blogs where blog = ?" blog)
      (wse-exec "update blogs set last_id = ? where blog = ?" id blog)
    (wse-exec "insert into blogs(blog, last_id) values(?, ?)" blog id)))

(defun wse--initialize ()
  (unless wse--db
    (setq wse--db (sqlite-open
		   (expand-file-name "wse.sqlite" user-emacs-directory)))

    ;; Keeping track of ids per blog.
    (wse-exec "create table if not exists blogs (blog text primary key, last_id integer, last_comment_id integer)")

    ;; Statistics.
    (wse-exec "create table if not exists views (id integer primary key, blog text, date date, time datetime, page text, ip text, user_agent text, title text, country text, referrer text, browser text, os text, type text, unique_page text)")
    (wse-exec "create table if not exists referrers (id integer primary key, blog text, time datetime, referrer text, page text)")
    (wse-exec "create table if not exists clicks (id integer primary key, blog text, time datetime, click text, domain text, page text)")

    ;; History.
    (wse-exec "create table if not exists history (id integer primary key, blog text, date date, views integer, visitors integer, clicks integer, referrers integer)")
    (wse-exec "create unique index if not exists historyidx1 on history(blog, date)")

    ;; Countries.
    (wse-exec "create table if not exists country_counter (id integer)")
    (unless (wse-sel "select * from country_counter")
      (wse-exec "insert into country_counter values (0)"))
    (wse-exec "create table if not exists countries (code text primary key, name text)")

    ;; Comments.
    (wse-exec "create table if not exists comments (blog text, id integer, post_id integer, time datetime, author text, email text, url text, content text, status text)")
    (wse-exec "create unique index if not exists commentsidx1 on comments(blog, id)")))

(defun wse--insert-data (blog time click page referrer ip user-agent title)
  ;; Titles aren't set for clicks.
  (when (eq title :null)
    (setq title ""))
  (when (wse--url-p page)
    (if (wse--url-p click)
	;; Register a click if it's not going to the current blog, or
	;; whether it's going to a media URL of some kind (image/mp4/etc).
	(when (or (not (member (wse--host click) wse-blogs))
		  (string-match "/wp-contents/uploads/" click)
		  (wse--media-p click))
	  (wse-exec
	   "insert into clicks(blog, time, click, domain, page) values(?, ?, ?, ?, ?)"
	   blog time click (wse--host click) page))
      ;; Insert into views.
      (wse-exec
       "insert into views(blog, date, time, page, ip, user_agent, title, country, referrer, unique_page) values(?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
       blog (substring time 0 10) time page ip user-agent title ""
       referrer (wse--clean-url page))
      ;; Check whether to register a referrer.
      (when (and (wse--url-p referrer)
		 (not (equal (wse--host referrer) blog)))
	(wse-exec
	 "insert into referrers(blog, time, referrer, page) values(?, ?, ?, ?)"
	 blog time referrer page)))))

(defun wse--store-comments (blog comments)
  (cl-loop for comment across comments
	   do (wse-exec "update blogs set last_comment_id = ? where blog = ?"
			(gethash "comment_id" comment)
			blog)
	   if (wse-sel "select id from comments where id = ? and blog = ?"
		       (gethash "comment_id" comment)
		       blog)
	   ;; We're selecting on comment_id now, so we'll never get
	   ;; updated statuses...
	   do (wse-exec "update comments set status = ? where blog = ? and id = ?"
			(gethash "comment_approved" comment)
			blog
			(gethash "comment_id" comment))
	   else
	   do (wse-exec "insert into comments(blog, id, post_id, time, author, email, url, content, status) values(?, ?, ?, ?, ?, ?, ?, ?, ?)"
			blog
			(gethash "comment_id" comment)
			(gethash "comment_post_id" comment)
			(gethash "comment_date_gmt" comment)
			(gethash "comment_author" comment)
			(gethash "comment_author_email" comment)
			(gethash "comment_url" comment)
			(gethash "comment_content" comment)
			(gethash "comment_approved" comment))))

(defun wse--possibly-summarize-history ()
  (let ((max (caar (wse-sel "select max(date) from history"))))
    (when (or (not max)
	      (string< max (substring
			    (wse--time (- (time-convert (current-time) 'integer)
					  (* 60 60 24)))
			    0 10)))
      (wse--summarize-history))))

(defun wse--summarize-history ()
  (dolist (blog wse-blogs)
    (cl-loop with max-date = (caar (wse-sel "select max(date) from views"))
	     for (date views visitors) in
	     (wse-sel "select date, count(date), count(distinct ip) from views where date < ? and blog = ? group by date order by date"
		      max-date blog)
	     unless (wse-sel "select date from history where blog = ? and date = ?"
			     blog date)
	     do (wse-exec "insert into history(blog, date, views, visitors, clicks, referrers) values (?, ?, ?, ?, ?, ?)"
			  blog date views visitors
			  (caar (wse-sel "select count(*) from clicks where blog = ? and time between ? and ?"
					 blog (concat date " 00:00:00")
					 (concat date " 23:59:59")))
			  (caar (wse-sel "select count(*) from referrers where blog = ? and time between ? and ?"
					 blog (concat date " 00:00:00")
					 (concat date " 23:59:59")))))))

(defun wse--fill-country ()
  (setq wse--filling-country t)
  (let ((id (or (caar (wse-sel "select id from country_counter"))
		0))
	func)
    (setq func
	  (lambda ()
	    (let ((next
		   (caar (wse-sel "select min(id) from views where id > ?"
				  id))))
	      (if (not next)
		  (setq wse--filling-country nil)
		(url-retrieve
		 (format "http://ip-api.com/json/%s"
			 (caar (wse-sel "select ip from views where id = ?"
					next)))
		 (lambda (status)
		   (goto-char (point-min))
		   (unwind-protect
		       (let ((country-code "-")
			     (country-name nil))
			 (when (and (not (plist-get status :error))
				    (search-forward "\n\n" nil t))
			   (let ((json (json-parse-buffer)))
			     (when (equal (gethash "status" json) "success")
			       (setq country-code (gethash "countryCode" json)
				     country-name (gethash "country" json)))))
			 (kill-buffer (current-buffer))
			 (wse-exec "update views set country = ? where id = ?"
				   country-code next)
			 (wse-exec "update country_counter set id = ?" next)
			 (when (and country-name
				    (not (wse-sel "select * from countries where code = ?"
						  country-code)))
			   (wse-exec "insert into countries(code, name) values (?, ?)"
				     country-code country-name))
			 (setq id next))
		     ;; The API is rate limited at 45 per minute, so
		     ;; poll max 30 times per minute.
		     (run-at-time 2 nil func)))
		 nil t)))))
    (funcall func)))

(defun wse--fill-browser ()
  (cl-loop for (id user-agent) in (wse-sel "select id, user_agent from views where type is null order by id")
	   for data =
	   (with-temp-buffer
	     (call-process (wse--file "detect-browser.pl") nil t nil
			   (if (zerop (length user-agent))
			       "Mozilla"
			     user-agent))
	     (goto-char (point-min))
	     (json-parse-buffer :null-object nil))
	   do (wse-exec "update views set browser = ?, os = ?, type = ? where id = ?"
			(gethash "browser" data
				 (cond
				  ((gethash "robot" data)
				   "robot")
				  ((gethash "lib" data)
				   "lib")
				  (t
				   "")))
			(gethash "OS" data "")
			(cond
			 ((equal (gethash "mobile" data) "1")
			  "M")
			 ((equal (gethash "robot" data) "1")
			  "R")
			 ((equal (gethash "lib" data) "1")
			  "L")
			 ((equal (gethash "tablet" data) "1")
			  "T")
			 (t
			  "N"))
			id)))

;; Modes and command for modes.

(defvar-keymap wse-mode-map
  :parent button-map
  "g" #'wse-revert
  "c" #'wse-display-comments
  "d" #'wse-view-date
  "q" #'bury-buffer
  "w" #'wse-clicks-view-todays-media
  "u" #'wse-view-user-agents
  "v" #'wse-view-details
  "p" #'wse-make-pingback
  "t" #'wse-view-top-pages)

(define-derived-mode wse-mode special-mode "WSE"
  "Major mode for listing Wordpress statistics."
  :interactive nil
  (setq truncate-lines t))

(defun wse-revert (&optional silent)
  "Update the current buffer."
  (interactive nil wse-mode)
  (unless silent
    (message "Updating..."))
  (let ((buffer (current-buffer)))
    (wse--poll-blogs
     (lambda ()
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (wse--render)
	   (unless silent
	     (message "Updating...done"))))))))

(defun wse-view-date (date)
  (interactive
   (list (completing-read
	  "Date to show: "
	  (mapcar #'car (wse-sel "select distinct date from history order by date"))))
   wse-mode)
  (switch-to-buffer "*WSE Date*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (wse--view-total-views nil date)
    (goto-char (point-max))
    (insert "\n")
    (wse--view-total-referrers nil date)
    (goto-char (point-max))
    (insert "\n")
    (wse--view-total-clicks nil date)
    (goto-char (point-max))
    (insert "\n")
    (wse--view-total-countries nil date)
    (goto-char (point-min))))

(defun wse-view-details ()
  "View details of the URL under point."
  (interactive nil wse-mode)
  (let ((callback (get-text-property (point) 'wse--details)))
    (unless callback
      (user-error "No details here"))
    (funcall callback)))

(defun wse-display-comments ()
  "Display recent comments, even spam comments."
  (interactive)
  (switch-to-buffer "*WSE Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (wse--render-comments t)))

(defun wse--view-page-details (urls &optional cutoff)
  (when (stringp urls)
    (setq urls (list urls)))
  (switch-to-buffer "*WSE Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'wse
     :columns '((:name "Time")
		(:name "Page" :max-width 15)
		(:name "IP" :max-width 20)
		(:name "Referrer" :max-width 40)
		(:name "Country")
		(:name "User-Agent"))
     :objects (apply
	       #'wse-sel
	       (format "select time, page, ip, referrer, country, user_agent from views where time > ? and page in (%s) order by time"
		       (wse--in urls))
	       (or cutoff (wse--24h)) urls)
     :getter #'wse--details-get
     :keymap wse-mode-map)))

(defun wse--details-get (elem column vtable)
  (cond
   ((member (vtable-column vtable column) '("Referrer" "Page" "Click"))
    (wse--possibly-buttonize (elt elem column)))
   ((equal (vtable-column vtable column) "Time")
    (wse--local-time (elt elem column)))
   (t
    (elt elem column))))

(defun wse--view-select-details (statement param)
  (switch-to-buffer "*WSE Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'wse
     :columns '((:name "Time")
		(:name "Page" :max-width 40)
		(:name "Referrer" :max-width 40)
		(:name "IP" :max-width 20)
		(:name "Country")
		(:name "User-Agent"))
     :objects (apply
	       #'wse-sel
	       (format "select time, page, referrer, ip, country, user_agent from views where time > ? and %s order by time"
		       statement)
	       (list (wse--24h) param))
     :getter #'wse--details-get
     :keymap wse-mode-map)))

(defun wse--view-referrer-details (urls)
  (switch-to-buffer "*WSE Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'wse
     :columns '((:name "Time")
		(:name "Referrer" :max-width 40)
		(:name "Page"))
     :objects (apply #'wse-sel
		     (format "select time, referrer, page from referrers where time > ? and referrer in (%s) order by time"
			     (wse--in urls))
		     (wse--24h) urls)
     :getter #'wse--details-get
     :keymap wse-mode-map)))

(defun wse--view-click-details (domain)
  (switch-to-buffer "*WSE Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'wse
     :columns '((:name "Time")
		(:name "Page" :max-width 40)
		(:name "Click"))
     :objects (wse-sel "select time, page, click from clicks where time > ? and domain = ? order by time"
		       (wse--24h) domain)
     :getter #'wse--details-get
     :keymap wse-mode-map)))

(defun wse--render ()
  (wse--initialize)
  (wse-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :face 'wse
     :use-header-line nil
     :columns '((:name "" :align 'right :min-width "70px")
		(:name "Views Today" :width "580px")
		(:name "" :align 'right :min-width "70px")
		(:name "Views Now" :width "580px"))
     :objects (wse--get-page-table-data)
     :keymap wse-mode-map)

    (goto-char (point-max))
    (insert "\n")
    (make-vtable
     :face 'wse
     :use-header-line nil
     :columns '((:name "" :align 'right :min-width "70px")
		(:name "Referrers" :width "580px")
		(:name "" :align 'right :min-width "70px")
		(:name "Clicks" :width "580px"))
     :objects (wse--get-click-table-data)
     :getter
     (lambda (elem column _vtable)
       (wse--possibly-buttonize (elt elem column)))
     :keymap wse-mode-map)

    (goto-char (point-max))
    (insert "\n")
    (wse--render-comments)

    (goto-char (point-max))
    (insert "\n")
    (insert-image (svg-image (wse--world-map)
			     :max-width (- (frame-pixel-width) 100))
		  "*")
    (insert "\n")

    (goto-char (point-max))
    (insert "\n")
    (make-vtable
     :face 'wse
     :use-header-line nil
     :columns '((:name "" :align 'right :min-width "70px")
		(:name "Countries" :width "300px")
		(:name "" :align 'right :min-width "70px")
		(:name "Browser" :width "200px")
		(:name "" :align 'right :min-width "70px")
		(:name "OS" :width "200px")
		(:name "" :align 'right :min-width "70px")
		(:name "Device" :width "200px"))
     :objects (wse--get-browser-table-data)
     :keymap wse-mode-map)

    (let* ((date (propertize
		  (let ((system-time-locale "C"))
		    (format-time-string "%a, %d %b %Y %T" (current-time)))
		  'face 'wse))
	   (dwidth (string-pixel-width date)))
      (setq header-line-format
	    (concat
	     (propertize " " 'display
			 (list 'space :width (list (- (window-width nil t)
						      dwidth))))
	     date)))

    (goto-char (point-min))
    (insert "\n")
    (goto-char (point-min))
    ;; We want to be able to put point somewhere unobtrusive.
    (insert (propertize " " 'display '(space :width (1))))
    (wse--plot-history)
    (wse--plot-blogs-today)
    (goto-char (point-min))))

(defun wse--render-comments (&optional display-spam)
  (when-let (comments (wse-sel
		       (format "select time, blog, status, author, content, post_id from comments where time > ? %s order by time desc"
			       (if display-spam
				   ""
				 "and status <> 'spam'"))
		       (if display-spam
			   (wse--24h)
			 (wse--time (- (time-convert (current-time) 'integer)
				       (* 2 60 60 24))))))
    (make-vtable
     :face 'wse
     :use-header-line display-spam
     :columns '((:name "Time")
		(:name "Blog" :max-width 20)
		(:name "Status")
		(:name "Author" :max-width "200px")
		(:name "Comment" :max-width 100))
     :objects comments
     :getter
     (lambda (elem column vtable)
       (wse--add-details
	;; Make the `v' command go to the ewp buffer for comments (so
	;; that you can reply to them and stuff).  This requires the
	;; ewp package.
	'ewp-list-comments
	(list (nth 1 elem))
	(cond
	 ((equal (vtable-column vtable column) "Status")
	  (cond
	   ((equal (elt elem column) "1")
	    "")
	   ((equal (elt elem column) "0")
	    "Pending")
	   (t
	    (elt elem column))))
	 ((equal (vtable-column vtable column) "Time")
	  (wse--local-time (elt elem column)))
	 ((equal (vtable-column vtable column) "Author")
	  (mm-url-decode-entities-string (elt elem column)))
	 ((equal (vtable-column vtable column) "Comment")
	  (let ((url (format "https://%s/?p=%d"
			     (elt elem 1) (elt elem 5))))
	    (buttonize (string-replace
			"\n" " "
			(with-temp-buffer
			  (insert (elt elem column))
			  (shr-render-region (point-min) (point-max))
			  (buffer-substring-no-properties
			   (point-min) (point-max))))
		       #'wse--browse url url)))
	 (t
	  (elt elem column)))))
     :keymap wse-mode-map)))

(defun wse--get-browser-table-data ()
  (let ((browsers (wse-sel "select count(browser), browser from views where time > ? group by browser order by count(browser) desc limit ?"
			   (wse--24h) wse-entries))
	(oses (wse-sel "select count(os), os from views where time > ? group by os order by count(os) desc limit ?"
		       (wse--24h) wse-entries))
	(types (wse-sel "select count(type), type from views where time > ? group by type order by count(type) desc limit ?"
			(wse--24h) wse-entries))
	(countries
	 (wse-sel "select count(country), name, code from views, countries where time > ? and views.country = countries.code group by country order by count(country) desc limit ?"
		  (wse--24h) wse-entries)))
    (nconc
     (cl-loop for i from 0 upto (1- wse-entries)
	      for country = (elt countries i)
	      for browser = (wse--filter-zero (elt browsers i))
	      for os = (wse--filter-zero (elt oses i))
	      for type = (wse--filter-zero (elt types i))
	      when (or country browser os type)
	      collect (append
		       (if country
			   (list (car country)
				 (wse--countrify
				  (nth 2 country) (nth 1 country)))
			 '("" ""))
		       (or browser '("" ""))
		       (if os
			   (list (car os)
				 (capitalize (cadr os)))
			 '("" ""))
		       (let ((type type))
			 (if type
			     (list (car type)
				   (cond
				    ((equal (cadr type) "N")
				     "Desktop")
				    ((equal (cadr type) "T")
				     "Tablet")
				    ((equal (cadr type) "M")
				     "Mobile")
				    (t
				     (cadr type))))
			   '("" "")))))
     (list
      (list
       (caar (wse-sel "select count(distinct country) from views where time > ?"
		      (wse--24h)))
       (buttonize "Total Countries" #'wse--view-total-countries)
       "" "" "" "" "" "")))))

(defun wse--filter-zero (elem)
  (if (equal (car elem) 0)
      nil
    elem))

(defun wse--clean-url (url)
  (and url
       (replace-regexp-in-string
	"#.*\\'" ""
	(replace-regexp-in-string
	 "/page/[0-9]+/\\'" "/"
	 (replace-regexp-in-string
	  "\\?fbclid.*\\|\\?from=.*utm_.*\\|\\?utm_.*" ""
	  url)))))

(defun wse--transform-pages (data cutoff)
  (let ((counts (make-hash-table :test #'equal))
	(titles (make-hash-table :test #'equal))
	(urls (make-hash-table :test #'equal))
	(results nil))
    (cl-loop for (count title url) in data
	     for page = (wse--clean-url url)
	     do
	     (cond
	      ((string= (url-filename (url-generic-parse-url page)) "/")
	       (setq page "/"
		     title "Home Page"))
	      ((string-match-p "\\`/\\(page/[0-9+]/\\)?[?]s="
			       (url-filename (url-generic-parse-url page)))
	       (setq page "/s"
		     title "Search")))
	     (cl-pushnew url (gethash page urls nil) :test #'equal)
	     (cl-incf (gethash page counts 0) count)
	     (setf (gethash page titles) title))
    (maphash (lambda (page count)
	       (let ((title (gethash page titles))
		     (urls (gethash page urls)))
		 (push
		  (list
		   count
		   (wse--add-details
		    #'wse--view-page-details
		    (list urls cutoff)
		    (buttonize
		     (cond
		      ((and (length> urls 1)
			    (or (equal title "Home Page")
				(equal title "Search")))
		       (concat "🔽 "
			       (buttonize
				title
				(lambda (_)
				  (wse--view-page-details urls cutoff)))))
		      ((wse--url-p title)
		       (wse--pretty-url title))
		      ((zerop (length title))
		       (wse--pretty-url (car urls)))
		      (t
		       (wse--adjust-title title (car urls))))
		     #'wse--browse (car urls)
		     (car urls)))
		   (car urls))
		  results)))
	     counts)
    (nreverse (sort results #'car-less-than-car))))

(defun wse--select-views (cutoff)
  (seq-take
   (wse--sort-views
    (wse--transform-pages
     (wse-sel "select count(page), title, page from views where time > ? group by page order by count(page) desc"
	      cutoff)
     cutoff))
   wse-entries))
	  
(defun wse--get-page-table-data ()
  (nconc
   (cl-loop with today = (wse--select-views (wse--24h))
	    and now = (wse--select-views (wse--1h))
	    for i from 0 upto (1- wse-entries)
	    when (or (elt today i) (elt now i))
	    collect
	    (cl-loop
	     for page in (list (elt today i) (elt now i))
	     append (if page
			(seq-take page 2)
		      (list "" ""))))
   (list
    (list
     (caar (wse-sel "select count(*) from views where time > ?" (wse--24h)))
     (buttonize "Total Views" #'wse--view-total-views (wse--24h))
     (caar (wse-sel "select count(*) from views where time > ?" (wse--1h)))
     (buttonize "Total Views" #'wse--view-total-views (wse--1h))))))

(defun wse--sort-views (views)
  (sort views
	(lambda (v1 v2)
	  (cond
	   ((< (car v1) (car v2))
	    nil)
	   ((> (car v1) (car v2))
	    t)
	   (t
	    (let ((v1-date (wse--url-date (nth 2 v1)))
		  (v2-date (wse--url-date (nth 2 v2))))
	      (cond
	       ((and v1-date v2-date)
		(string> v1-date v2-date))
	       (v1-date
		t)
	       (v2-date
		nil)
	       (t
		t))))))))

(defun wse--url-date (url)
  (and (string-match "/[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9]/" url)
       (match-string 0 url)))

(defun wse--add-details (callback params string)
  (propertize string
	      'wse--details
	      (lambda ()
		(apply callback params))))

(defun wse--add-media-clicks (clicks)
  (let ((images nil)
	(videos nil))
    (cl-loop for (click)
	     in
	     (apply
	      #'wse-sel
	      (format
	       "select click from clicks where time > ? and domain in (%s)"
	       (wse--in wse-blogs))
	      (wse--24h) wse-blogs)
	     if (string-match-p "[.]mp4\\'" click)
	     do (push click videos)
	     else
	     do (push click images))
    (when images
      (push (list (length images)
		  (concat
		   "🔽 " (buttonize "Images" #'wse--view-clicks images)))
	    clicks))
    (when videos
      (push (list (length videos)
		  (concat
		   "🔽 " (buttonize "Videos" #'wse--view-clicks videos)))
	    clicks))
    (nreverse (sort clicks #'car-less-than-car))))

(defun wse--get-click-table-data ()
  (let* ((time (wse--24h))
	 (referrers
	  (wse--transform-referrers
	   (wse-sel "select count(referrer), referrer from referrers where time > ? group by referrer order by count(referrer) desc"
		    time)
	   t))
	 (clicks
	  (wse--add-media-clicks
	   (apply
	    #'wse-sel
	    (format "select count(domain), domain, count(distinct click), click from clicks where time > ? and domain not in (%s) group by domain order by count(domain) desc limit ?"
		    (wse--in wse-blogs))
	    `(,time ,@wse-blogs ,wse-entries)))))
    (nconc
     (cl-loop for i from 0 upto (1- wse-entries)
	      for click = (elt clicks i)
	      for referrer = (elt referrers i)
	      when (or click referrer)
	      collect
	      (append
	       (if referrer
		   (list (car referrer)
			 (wse--add-details
			  #'wse--view-referrer-details (list (nth 2 referrer))
			  (wse--possibly-buttonize (cadr referrer))))
		 '("" ""))
	       (if click
		   (list (car click)
			 (cond
			  ((not (nth 2 click))
			   (wse--add-details
			    #'wse--view-click-details (list (nth 1 click))
			    (nth 1 click)))
			  ((= (nth 2 click) 1)
			   (wse--add-details
			    #'wse--view-click-details (list (nth 1 click))
			    (nth 3 click)))
			  (t
			   (concat
			    "🔽 " (buttonize
				   (cadr click)
				   (lambda (domain)
				     (wse--view-clicks domain))
				   (cadr click))))))
		 '("" ""))))
	       
     (list
      (list
       (caar (wse-sel "select count(*) from referrers where time > ?" time))
       (buttonize "Total Referrers" #'wse--view-total-referrers)
       (caar (wse-sel "select count(*) from clicks where time > ?" time))
       (buttonize "Total Clicks" #'wse--view-total-clicks))))))

(defvar-keymap wse-clicks-mode-map
  :parent button-map
  "w" #'wse-clicks-view-todays-media
  "q" #'bury-buffer)

(define-derived-mode wse-clicks-mode special-mode
  :interactive nil
  (setq truncate-lines t))

(defvar wse--shown-media (make-hash-table :test #'equal))

(defun wse-clicks-view-todays-media ()
  "View today's media clicks."
  (interactive nil wse-clicks-mode wse-mode)
  (let* ((urls (cl-loop for (url)
			in (wse-sel "select distinct click from clicks where time > ?"
				    (wse--24h))
			when (and (and (wse--media-p url)
				       (not (string-match "[.]mp4\\'" url)))
				  (string-match-p
				   (format-time-string "/uploads/%Y/%m/")
				   url)
				  (not (gethash url wse--shown-media)))
			collect url)))
    (unless urls
      (error "No new unseen media clicks"))
    (dolist (url urls)
      (setf (gethash url wse--shown-media) t))
    (open-webs urls)))

(defun wse--view-clicks (domains)
  (unless (listp domains)
    (setq domains (list domains)))
  (switch-to-buffer "*Clicks*")
  (wse-clicks-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'wse
     :columns '((:name "" :align 'right)
		(:name "Clicks"))
     :objects (if (string-match "\\`http" (car domains))
		  ;; In this case it's really a list of URLs, not domains.
		  (apply
		   #'wse-sel
		   (format "select count(click), click from clicks where time > ? and click in (%s) group by click order by count(click) desc"
			   (wse--in domains))
		   (wse--24h) domains)
		(apply
		 #'wse-sel
		 (format "select count(click), click from clicks where time > ? and domain in (%s) group by click order by count(click) desc"
			 (wse--in domains))
		 (wse--24h) domains))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (wse--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap wse-clicks-mode-map)))

(defun wse--view-total-views (cutoff &optional date)
  (unless date
    (switch-to-buffer "*Total WSE*"))
  (let ((inhibit-read-only t)
	(from (or cutoff (wse--24h)))
	(to (wse--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'wse
     :columns '((:name "" :align 'right)
		(:name "Blog" :max-width 20)
		(:name "Posts & Pages"))
     :objects (wse-sel "select count(page), blog, title, page from views where time > ? and time <= ? group by page order by count(page) desc, id"
		       from to)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Posts & Pages")
	   (buttonize (wse--adjust-title (elt elem column)
					 (elt elem 3))
		      #'wse--browse (elt elem 3) (elt elem 3))
	 (elt elem column)))
     :keymap wse-mode-map)))

(defun wse--view-total-referrers (_ &optional date)
  (unless date
    (switch-to-buffer "*Total WSE*"))
  (let ((inhibit-read-only t)
	(from (wse--24h))
	(to (wse--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'wse
     :columns '((:name "" :align 'right)
		(:name "Referrers"))
     :objects (wse--transform-referrers
	       (wse-sel "select count(referrer), referrer from referrers where time > ? and time <= ? group by referrer order by count(referrer) desc"
			from to))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Referrers")
	   (wse--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap wse-mode-map)))

(defun wse--view-total-clicks (_ &optional date)
  (unless date
    (switch-to-buffer "*Total WSE*"))
  (let ((inhibit-read-only t)
	(from (wse--24h))
	(to (wse--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'wse
     :columns '((:name "" :align 'right)
		(:name "Clicks"))
     :objects (wse-sel "select count(distinct click), click from clicks where time > ? and time <= ? group by click order by count(click) desc"
		       from to)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (wse--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap wse-mode-map)))

(defun wse--view-total-countries (_ &optional date)
  (unless date
    (switch-to-buffer "*Total WSE*"))
  (let ((inhibit-read-only t)
	(from (wse--24h))
	(to (wse--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'wse
     :columns '((:name "" :align 'right)
		(:name "Countries"))
     :objects (wse-sel "select count(country), name, code from views, countries where time > ? and time <= ? and views.country = countries.code group by country order by count(country) desc"
		       from to)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Countries")
	   (wse--add-details
	    #'wse--view-select-details
	    (list "country = ?" (elt elem 2))
	    (wse--countrify (elt elem 2) (elt elem column)))
	 (elt elem column)))
     :keymap wse-mode-map)))

(defun wse--transform-referrers (referrers &optional summarize)
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for (count url) in referrers
	     for trans = (wse--transform-referrer url summarize)
	     ;; OK, OK, this is stupid, but...
	     do (cl-loop repeat count
			 do (push url (gethash trans table nil))))
    (let ((result nil))
      (maphash (lambda (referrer urls)
		 (let ((length (length urls)))
		   (cond
		    ((= (elt referrer 0) ?-)
		     (if (= (length (seq-uniq urls #'equal)) 1)
			 (push (list length (car urls) urls) result)
		       (push (list
			      length
			      (concat "🔽 "
				      (buttonize
				       (substring referrer 1)
				       (lambda (urls)
					 (wse--view-referrer-details urls))
				       urls))
			      urls)
			     result)))
		    (t
		     (push (list length referrer urls) result)))))
	       table)
      (let ((list (nreverse (sort result #'car-less-than-car))))
	(if summarize
	    (seq-take list wse-entries)
	  list)))))

(defvar wse--search-engines
  '("Bing"
    "Google"
    "Baidu"
    "Presearch"
    "Yahoo"
    "DuckDuckGo"
    "Yandex"
    ("ya" "Yandex")
    "Qwant"
    "Kagi"
    "Ecosia"
    "Mojeek"
    ("pch" "PCH" "search.pch.com")
    ("brave" "Brave" "search.brave.com")
    ("yahoo" "Yahoo" "search.yahoo.com")
    ("aol" "AOL" "search.aol.com")))

(defun wse--search-p (entity host)
  (cl-loop for elem in wse--search-engines
	   for name = (if (consp elem)
			  (cadr elem)
			elem)
	   for bit = (if (consp elem)
			  (car elem)
			(downcase elem))
	   when (or (equal entity bit)
		    (and (consp elem)
			 (equal host (nth 2 elem))))
	   return name))

(defun wse--transform-referrer (url &optional summarize)
  (let* ((domain (wse--get-domain (wse--host url)))
	 (entity (downcase (car (split-string domain "[.]"))))
	 (search (wse--search-p entity (wse--host url))))
    (if search
	(if summarize
	    "Search"
	  search)
      (cond
       ((string-match-p "[.]pinterest[.]com/\\'" url)
	"Pinterest")
       ((string-match-p "[.]?bsky[.][a-z]+/\\'" url)
	"Bluesky")
       ((string-match-p "[a-z]+[.]wikipedia[.]org/\\'" url)
	"Wikipedia")
       ((and summarize (string-match-p "\\ampproject[.]org/" url))
	"Amp")
       ((equal (wse--host url) "t.co")
	"Twitter")
       ((equal (wse--get-domain (wse--host url)) "facebook.com")
	"Facebook")
       ((and summarize (member (wse--host url) wse-blogs))
	"Interblog")
       ((string-match-p "\\bstatics.teams.cdn.office.net/\\'" url)
	"Microsoft Teams")
       (summarize
	(concat "-" (wse--get-domain (wse--host url))))
       (t
	url)))))

;; Plots.

(defun wse--plot-blogs-today ()
  (let ((data 
	 (wse-sel "select blog, count(blog), count(distinct ip) from views where time > ? group by blog order by blog"
		  (wse--24h))))
    (insert-image
     (svg-image
      (eplot-make-plot
       `((Format horizontal-bar-chart)
	 (Color "#008000")
	 (Mode dark)
	 (Layout compact)
	 (Font ,wse-font)
	 (Margin-Left 10)
	 (Horizontal-Label-Left 15)
	 (Horizontal-Label-Font-Size 18)
	 (Height 300)
	 (Width 250))
       (append
	'((Bar-Max-Width: 40))
	(cl-loop for (blog views _visitors) in data
		 collect (list views "# Label: " blog)))
       (append
	'((Bar-Max-Width: 40)
	  (Color: "#006000"))
	(cl-loop for (blog _views visitors) in data
		 collect (list visitors "# Label: " blog)))))
     "*")))

(defun wse--plot-history ()
  (let ((data (nreverse (wse-sel "select date, sum(views), sum(visitors) from history group by date order by date desc limit 14")))
	(today (car (wse-sel "select count(*), count(distinct ip) from views where time > ?"
			     (wse--24h))))
	(current
	 (car (wse-sel
	       "select count(*), count(distinct ip) from views where time > ?"
	       (format-time-string "%Y-%m-%d 00:00:00" nil "Z")))))
    (insert-image
     (svg-image
      (eplot-make-plot
       `((Mode dark)
	 (Layout compact)
	 (Font ,wse-font)
	 (Height 300)
	 (Width 550)
	 (Format bar-chart))
       (append
	'((Bar-Max-Width: 40)
	  (Color: "#006000"))
	(cl-loop for (date _views visitors) in data
		 collect (list
			  visitors "# Label: " (substring date 8)
			  (wse--label-font-weight date)))
	(list (list (cadr current) "# Label: "
		    (format-time-string "%d" nil "Z")
		    (wse--label-font-weight
		     (format-time-string "%Y-%m-%d" nil "Z"))))
	(list (list (cadr today) "# Label: 24h, Label-Font-Weight: normal")))
       (append
	'((Bar-Max-Width: 40)
	  (Color: "#008000"))
	(cl-loop for (date views _visitors) in data
		 collect (list views "# Label: " (substring date 8)))
	(list (list (car current) "# Label: "
		    (format-time-string "%d" nil "Z")))
	(list (list (car today) "# Label: 24h")))))
     "*")))

(defun wse--label-font-weight (date)
  (format ", Label-Font-Weight: %s"
	  (if (wse--weekend-p date)
	      "bold"
	    "normal")))

(defun wse--world-map ()
  (let* ((data 
	  (wse-sel "select count(country), code from views, countries where time > ? and views.country = countries.code group by country order by count(country) desc"
		   (wse--24h)))
	 (max (and data (log (caar data))))
	 (svg (with-temp-buffer
		(insert-file-contents (wse--file "world.svg"))
		(car (dom-by-tag (libxml-parse-xml-region
				  (point) (point-max))
				 'svg)))))
    (dom-set-attribute svg 'fill "#202020")
    (when (and max
	       (not (zerop max)))
      (cl-loop for (views code) in data
	       for elems = (dom-by-class svg (concat "\\`" code "\\'"))
	       do (cl-loop for elem in elems
			   for col = (+ 40 (truncate
					    (* (/ (log views) max) 210)))
			   do (dom-set-attribute
			       elem 'fill
			       (format "#%02x%02x%02x" col col col)))))
    svg))

(defvar-keymap wse-user-agents-mode-map
  :parent button-map
  "d" #'wse-user-agent-delete
  "q" #'bury-buffer)

(define-derived-mode wse-user-agents-mode special-mode
  :interactive nil
  (setq truncate-lines t))

(defun wse-view-user-agents ()
  "Display today's User-Agent strings."
  (interactive)
  (switch-to-buffer "*WSE User-Agents*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (wse-user-agents-mode)
    (make-vtable
     :face 'wse
     :columns '("Count" "User-Agent")
     :objects (wse-sel
	       "select count(*), user_agent from views where time > ? group by user_agent order by count(*) desc"
	       (wse--24h)))))

(defun wse-user-agent-delete (user-agent)
  "Delete the User-Agent under point."
  (interactive (list (cadr (vtable-current-object))))
  (when (yes-or-no-p (format "Delete %s? " user-agent))
    (wse-exec "delete from views where user_agent = ?" user-agent)
    (vtable-remove-object (vtable-current-table) (vtable-current-object))))

(defun wse--update-unique-page ()
  (cl-loop for (id page) in (wse-sel "select id, page from views")
	   do (wse-exec "update views set unique_page = ? where id = ?"
			(wse--clean-url page) id )))

(defun wse-view-top-pages ()
  "View the 100 most popular pages."
  (interactive)
  (switch-to-buffer "*WSE Top*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'wse
     :columns '("Count" "Page")
     :objects (wse-sel "select count(unique_page), unique_page, title from views group by unique_page order by count(unique_page) desc, id limit 100")
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Page")
	   (buttonize (wse--adjust-title (elt elem 2)
					 (elt elem column))
		      #'wse--browse (elt elem column) (elt elem column))
	 (elt elem column)))
     :keymap wse-mode-map)))

(defun wse-make-pingback ()
  "Make a pingback for the current URL pairs."
  (interactive)
  (let* ((object (vtable-current-object))
	 (url (nth 1 object))
	 (post-url (nth 2 object)))
    (when (or (not url)
	      (not post-url)
	      (not (string-match "\\`https?:" url))
	      (not (string-match "\\`https?:" post-url)))
      (user-error "Not an URL pair on the current line"))
    (if (length> (url-filename (url-generic-parse-url url)) 1)
	;; We have a complete URL.
	(ewp-possibly-make-pingback post-url url)
      ;; We have an URL to just the domain, so find the actual URL.
      )))

(provide 'wse)

;;; wse.el ends here
