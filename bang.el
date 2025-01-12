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
(require 'eplot)
(require 'url-domsuf)

(defvar bang-font "sans-serif"
  "Font family to use in buffer and charts.")

(defface bang
  `((t :family ,bang-font))
  "The face to use in bang buffers.")

(defvar bang-blogs nil
  "A list of blogs to collect statistics from.
This should be a list of names (like \"foo.org\" and not URLs.")

(defvar bang-entries 12
  "The number of entries to display.")

;; Internal variables.
(defvar bang--db nil)
(defvar bang--filling-country nil)

(defun bang ()
  "Display Wordpress statistics."
  (interactive)
  (switch-to-buffer "*Bang*")
  (bang--render))


;; Helper functions.

(defun bang--bot-p (user-agent)
  (let ((case-fold-search t))
    (string-match-p "bot/\\|spider\\b" user-agent)))

(defun bang--host (url)
  (url-host (url-generic-parse-url url)))

(defun bang--url-p (string)
  (and (and (stringp string))
       (not (zerop (length string)))
       (string-match-p "\\`[a-z]+:" string)))

(defun bang--media-p (click)
  (string-match "[.]\\(mp4\\|png\\|jpg\\|jpeg\\|webp\\|webp\\)\\'" click))

(defun bang--countrify (code name)
  (if (= (length code) 2)
      ;; Convert the country code into a Unicode flag.
      (concat (string (+ #x1f1a5 (elt code 0)) (+ #x1f1a5 (elt code 1)))
	      " " name)
    name))

(defun bang--pretty-url (string)
  (replace-regexp-in-string "\\`[a-z]+://" "" string))

(defun bang--possibly-buttonize (string)
  (if (bang--url-p string)
      (buttonize (bang--pretty-url string) #'bang--browse string string)
    string))

(defun bang--time (time)
  (format-time-string "%Y-%m-%d %H:%M:%S" time))

(defun bang--now ()
  (bang--time (- (time-convert (current-time) 'integer)
		 (* 60 60 24))))

(defun bang--future ()
  "9999-12-12 23:59:00")

(defun bang--convert-time (time)
  "Convert TIME from GMT/Z/UTC to local time."
  (bang--time
   (encode-time (iso8601-parse (concat (string-replace " " "T" time) "Z")))))

(defun bang--browse (url)
  (let ((browse-url-browser-function
	 (if (and (bang--media-p url)
		  (not (string-match "[.]mp4\\'" url)))
	     browse-url-browser-function
	   browse-url-secondary-browser-function)))
    (browse-url url)))

(defun bang--get-domain (host)
  "Return the shortest domain that refers to an entity.
I.e., \"google.com\" or \"google.co.uk\"."
  (let* ((bits (reverse (split-string host "[.]")))
	 (domain (pop bits)))
    (cl-loop while (and bits
			(not (url-domsuf-cookie-allowed-p domain)))
	     do (setq domain (concat (pop bits) "." domain)))
    domain))

(defun bang-sel (statement &rest args)
  (sqlite-select bang--db statement args))

(defun bang-exec (statement &rest args)
  (sqlite-execute bang--db statement args))

;; Update data.

(defun bang--poll-blogs (&optional callback)
  (let ((blogs bang-blogs)
	(data nil)
	func)
    (setq func
	  (lambda ()
	    (let* ((blog (pop blogs))
		   (ids (or (car
			     (bang-sel "select last_id, last_comment_id from blogs where blog = ?"
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
		     (bang--update-data data callback))))
	       nil t))))
    (funcall func)))      

(defun bang--update-data (data &optional callback)
  (cl-loop for (blog . elems) in data
	   do (cl-loop for elem across (gethash "data" elems)
		       for (id time click page referrer ip user-agent title) =
		       (cl-coerce elem 'list)
		       unless (bang--bot-p user-agent)
		       do
		       (bang--insert-data blog (bang--convert-time time)
					  click page referrer ip
					  user-agent title)
		       (bang--update-id blog id))
	   do (bang--store-comments blog (gethash "comments" elems)))

  (unless bang--filling-country
    (bang--fill-country))
  (bang--possibly-summarize-history)
  (when callback
    (funcall callback)
    (message "Updating...done")))

(defun bang--update-id (blog id)
  (if (bang-sel "select last_id from blogs where blog = ?" blog)
      (bang-exec "update blogs set last_id = ? where blog = ?" id blog)
    (bang-exec "insert into blogs(blog, last_id) values(?, ?)" blog id)))

(defun bang--initialize ()
  (unless bang--db
    (setq bang--db (sqlite-open
		    (expand-file-name "bang.sqlite" user-emacs-directory)))

    ;; Keeping track of ids per blog.
    (bang-exec "create table if not exists blogs (blog text primary key, last_id integer, last_comment_id integer)")

    ;; Statistics.
    (bang-exec "create table if not exists views (id integer primary key, blog text, date date, time datetime, page text, ip text, user_agent text, title text, country text)")
    (bang-exec "create table if not exists referrers (id integer primary key, blog text, time datetime, referrer text, page text)")
    (bang-exec "create table if not exists clicks (id integer primary key, blog text, time datetime, click text, domain text, page text)")

    ;; History.
    (bang-exec "create table if not exists history (id integer primary key, blog text, date date, views integer, visitors integer, clicks integer, referrers integer)")
    (bang-exec "create unique index if not exists historyidx1 on history(blog, date)")

    ;; Countries.
    (bang-exec "create table if not exists country_counter (id integer)")
    (unless (bang-sel "select * from country_counter")
      (bang-exec "insert into country_counter values (0)"))
    (bang-exec "create table if not exists countries (code text primary key, name text)")

    ;; Comments.
    (bang-exec "create table if not exists comments (blog text, id integer, post_id integer, time datetime, author text, email text, url text, content text, status text)")
    (bang-exec "create unique index if not exists commentsidx1 on comments(blog, id)")))

(defun bang--insert-data (blog time click page referrer ip user-agent title)
  ;; Titles aren't set for clicks.
  (when (eq title :null)
    (setq title ""))
  (when (bang--url-p page)
    (if (bang--url-p click)
	;; Register a click if it's not going to the current blog, or
	;; whether it's going to a media URL of some kind (image/mp4/etc).
	(when (or (not (equal (bang--host click) blog))
		  (string-match "/wp-contents/uploads/" click)
		  (bang--media-p click))
	  (bang-exec
	   "insert into clicks(blog, time, click, domain, page) values(?, ?, ?, ?, ?)"
	   blog time click (bang--host click) page))
      ;; Insert into views.
      (bang-exec
       "insert into views(blog, date, time, page, ip, user_agent, title, country) values(?, ?, ?, ?, ?, ?, ?, ?)"
       blog (substring time 0 10) time page ip user-agent title "")
      ;; Check whether to register a referrer.
      (when (and (bang--url-p referrer)
		 (not (equal (bang--host referrer) blog)))
	(bang-exec
	 "insert into referrers(blog, time, referrer, page) values(?, ?, ?, ?)"
	 blog time referrer page)))))

(defun bang--store-comments (blog comments)
  (cl-loop for comment across comments
	   do (bang-exec "update blogs set last_comment_id = ? where blog = ?"
			 (gethash "comment_id" comment)
			 blog)
	   if (bang-sel "select id from comments where id = ? and blog = ?"
			(gethash "comment_id" comment)
			blog)
	   ;; We're selecting on comment_id now, so we'll never get
	   ;; updated statuses...
	   do (bang-exec "update comments set status = ? where blog = ? and id = ?"
			 (gethash "comment_approved" comment)
			 blog
			 (gethash "comment_id" comment))
	   else
	   do (bang-exec "insert into comments(blog, id, post_id, time, author, email, url, content, status) values(?, ?, ?, ?, ?, ?, ?, ?, ?)"
			 blog
			 (gethash "comment_id" comment)
			 (gethash "comment_post_id" comment)
			 (bang--convert-time
			  (gethash "comment_date_gmt" comment))
			 (gethash "comment_author" comment)
			 (gethash "comment_author_email" comment)
			 (gethash "comment_url" comment)
			 (gethash "comment_content" comment)
			 (gethash "comment_approved" comment))))

(defun bang--possibly-summarize-history ()
  (let ((max (caar (bang-sel "select max(date) from history"))))
    (when (or (not max)
	      (string< max (substring (bang--now) 0 10)))
      (bang--summarize-history))))

(defun bang--summarize-history ()
  (dolist (blog bang-blogs)
    (cl-loop with max-date = (caar (bang-sel "select date from views where blog = ? order by id desc limit 1"
					     blog))
	     for (date views visitors) in
	     (bang-sel "select date, count(date), count(distinct ip) from views where date < ? and blog = ? group by date order by date"
		       max-date blog)
	     unless (bang-sel "select date from history where blog = ? and date = ?"
			      blog date)
	     do (bang-exec "insert into history(blog, date, views, visitors, clicks, referrers) values (?, ?, ?, ?, ?, ?)"
			   blog date views visitors
			   (caar (bang-sel "select count(*) from clicks where blog = ? and time between ? and ?"
					   blog (concat date " 00:00:00")
					   (concat date " 23:59:59")))
			   (caar (bang-sel "select count(*) from referrers where blog = ? and time between ? and ?"
					   blog (concat date " 00:00:00")
					   (concat date " 23:59:59")))))))

(defun bang--fill-country ()
  (setq bang--filling-country t)
  (let ((id (or (caar (bang-sel "select id from country_counter"))
		0))
	func)
    (setq func
	  (lambda ()
	    (let ((next
		   (caar (bang-sel "select min(id) from views where id > ?"
				   id))))
	      (if (not next)
		  (setq bang--filling-country nil)
		(url-retrieve
		 (format "http://ip-api.com/json/%s"
			 (caar (bang-sel "select ip from views where id = ?"
					 next)))
		 (lambda (status)
		   (goto-char (point-min))
		   (let ((country-code "-")
			 (country-name nil))
		     (when (and (not (plist-get status :error))
				(search-forward "\n\n" nil t))
		       (let ((json (json-parse-buffer)))
			 (when (equal (gethash "status" json) "success")
			   (setq country-code (gethash "countryCode" json)
				 country-name (gethash "country" json)))))
		     (kill-buffer (current-buffer))
		     (bang-exec "update views set country = ? where id = ?"
				country-code next)
		     (bang-exec "update country_counter set id = ?" next)
		     (when (and country-name
				(not (bang-sel "select * from countries where code = ?"
					       country-code)))
		       (bang-exec "insert into countries(code, name) values (?, ?)"
				  country-code country-name))
		     (setq id next)
		     ;; The API is rate limited at 45 per minute, so
		     ;; poll max 30 times per minute.
		     (run-at-time 2 nil func)))
		 nil t)))))
    (funcall func)))

;; Modes and command for modes.

(define-derived-mode bang-mode special-mode "Bang"
  "Major mode for listing Wordpress statistics."
  :interactive nil
  (setq truncate-lines t))

(defvar-keymap bang-mode-map
  :parent button-map
  "g" #'bang-revert
  "d" #'bang-view-date
  "v" #'bang-view-details)

(defun bang-revert ()
  "Update the current buffer."
  (interactive nil bang-mode)
  (message "Updating...")
  (let ((buffer (current-buffer)))
    (bang--poll-blogs
     (lambda ()
       (when (buffer-live-p buffer)
	 (with-current-buffer buffer
	   (bang--render)))))))

(defun bang-view-date (date)
  (interactive
   (list (completing-read
	  "Date to show: "
	  (mapcar #'car (bang-sel "select distinct date from history order by date"))))
   bang-mode)
  (switch-to-buffer "*Bang Date*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (bang--view-total-views nil date)
    (goto-char (point-max))
    (insert "\n")
    (bang--view-total-referrers nil date)
    (goto-char (point-max))
    (insert "\n")
    (bang--view-total-clicks nil date)
    (goto-char (point-max))
    (insert "\n")
    (bang--view-total-countries nil date)
    (goto-char (point-min))))

(defun bang-view-details ()
  "View details of the URL under point."
  (interactive nil bang-mode)
  (cond
   ((eq (vtable-current-column) 1)
    (when-let ((data (elt (vtable-current-object) (vtable-current-column)))
	       (url (get-text-property 1 'help-echo data)))
      (bang--view-page-details url)))
   ((eq (vtable-current-column) 3)
    (when-let ((urls (elt (vtable-current-object) 4)))
      (when (listp urls)
	(bang--view-referrer-details urls))))))

(defun bang--view-page-details (url)
  (switch-to-buffer "*Bang Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'bang
     :columns '((:name "Time")
		(:name "IP" :max-width 20)
		(:name "Country")
		(:name "User-Agent"))
     :objects (bang-sel "select time, ip, country, user_agent from views where time > ? and page = ? order by time"
			(bang--now) url)
     :getter
     (lambda (elem column _vtable)
       (elt elem column))
     :keymap bang-mode-map)))

(defun bang--view-referrer-details (urls)
  (switch-to-buffer "*Bang Details*")
  (let ((inhibit-read-only t))
    (erase-buffer)
    (special-mode)
    (setq truncate-lines t)
    (make-vtable
     :face 'bang
     :columns '((:name "Time")
		(:name "Referrer" :max-width 40)
		(:name "Page"))
     :objects (apply #'bang-sel
		     (format "select time, referrer, page from referrers where time > ? and referrer in (%s) order by time"
			     (mapconcat (lambda (_) "?") urls ","))
		     (bang--now) urls)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Page")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--render ()
  (bang--initialize)
  (bang-mode)
  (let ((inhibit-read-only t))
    (erase-buffer)
    (make-vtable
     :face 'bang
     :use-header-line nil
     :columns '((:name "" :align 'right :min-width "70px")
		(:name "Posts & Pages" :width "600px")
		(:name "" :align 'right :min-width "70px")
		(:name "Referrers" :width 45))
     :objects (bang--get-page-table-data)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Referrers")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)

    (goto-char (point-max))
    (insert "\n")
    (make-vtable
     :face 'bang
     :use-header-line nil
     :columns '((:name "" :align 'right :min-width "70px")
		(:name "Clicks" :width "600px")
		(:name "" :align 'right :min-width "70px")
		(:name "Countries" :width 45))
     :objects (bang--get-click-table-data)
     :getter
     (lambda (elem column vtable)
       (cond
	((equal (vtable-column vtable column) "Clicks")
	 (bang--possibly-buttonize (elt elem column)))
	((equal (vtable-column vtable column) "Countries")
	 (bang--countrify (elt elem 4) (elt elem column)))
	(t
	 (elt elem column))))
     :keymap bang-mode-map)

    (goto-char (point-max))
    (insert "\n")
    (make-vtable
     :face 'bang
     :use-header-line nil
     :columns '((:name "Blog" :max-width 20)
		(:name "Status")
		(:name "Author")
		(:name "Comment" :max-width 60))
     :objects (bang-sel "select blog, status, author, content, post_id from comments where time > ? order by time desc"
			(bang--time (- (time-convert (current-time) 'integer)
				       (* 4 60 60 24))))
     :getter
     (lambda (elem column vtable)
       (cond
	((equal (vtable-column vtable column) "Status")
	 (if (equal (elt elem column) "1")
	     ""
	   (elt elem column)))
	((equal (vtable-column vtable column) "Comment")
	 (let ((url (format "https://%s/?p=%d"
			    (elt elem 0) (elt elem 4))))
	   (buttonize (elt elem column) #'bang--browse
		      url url)))
	(t
	 (elt elem column))))
     :keymap bang-mode-map)

    (goto-char (point-max))
    (insert "\n")
    (make-vtable
     :face 'bang
     :use-header-line nil
     :columns '((:name "Last Update"))
     :objects (list (message-make-date))
     :keymap bang-mode-map)

    (goto-char (point-min))
    (insert "\n")
    (goto-char (point-min))
    (bang--plot-history)
    (bang--plot-blogs-today)
    (insert "\n")))

(defun bang--get-page-table-data ()
  (let* ((time (bang--now))
	 (pages
	  (bang-sel "select count(page), title, page from views where time > ? group by page order by count(page) desc limit ?"
		    time bang-entries))
	 (referrers
	  (bang--transform-referrers
	   (bang-sel "select count(referrer), referrer from referrers where time > ? group by referrer order by count(referrer) desc"
		     time)
	   t)))
    (nconc
     (cl-loop for i from 0 upto (1- bang-entries)
	      for page = (elt pages i)
	      collect
	      (append (if page
			  (list (nth 0 page)
				(buttonize
				 (cond
				  ((bang--url-p (nth 1 page))
				   (bang--pretty-url (nth 1 page)))
				  ((zerop (length (nth 1 page)))
				   (bang--pretty-url (nth 2 page)))
				  (t
				   (nth 1 page)))
				 #'bang--browse (elt page 2)
				 (elt page 2)))
			(list "" ""))
		      (or (elt referrers i) (list "" ""))))
     (list
      (list
       (caar (bang-sel "select count(*) from views where time > ?" time))
       (buttonize "Total Views" #'bang--view-total-views)
       (caar (bang-sel "select count(*) from referrers where time > ?" time))
       (buttonize "Total Referrers" #'bang--view-total-referrers))))))

(defun bang--get-click-table-data ()
  (let* ((time (bang--now))
	 (clicks
	  (bang-sel "select count(domain), domain, count(distinct click), click from clicks where time > ? group by domain order by count(domain) desc limit ?"
		    time bang-entries))
	 (countries
	  (bang-sel "select count(country), name, code from views, countries where time > ? and views.country = countries.code group by country order by count(country) desc limit ?"
		    time bang-entries)))
    (nconc
     (cl-loop for i from 0 upto (1- bang-entries)
	      for click = (elt clicks i)
	      collect
	      (append (if click
			  (list (car click)
				(if (= (nth 2 click) 1)
				    (nth 3 click)
				  (buttonize
				   (concat "🔽 " (cadr click))
				   (lambda (domain)
				     (bang--view-clicks domain))
				   (cadr click))))
			(list "" ""))
		      (or (elt countries i) (list "" ""))))
    
     (list
      (list
       (caar (bang-sel "select count(*) from clicks where time > ?" time))
       (buttonize "Total Clicks" #'bang--view-total-clicks)
       (caar (bang-sel "select count(distinct country) from views where time > ?"
		       time))
       (buttonize "Total Countries" #'bang--view-total-countries))))))

(defvar-keymap bang-clicks-mode-map
  :parent button-map
  "v" #'bang-clicks-view-todays-media)

(define-derived-mode bang-clicks-mode special-mode
  :interactive nil
  (setq truncate-lines t))

(defvar bang--shown-media (make-hash-table :test #'equal))

(defun bang-clicks-view-todays-media ()
  "View today's media clicks."
  (interactive nil bang-clicks-mode)
  (let* ((objects
	 (save-excursion
	   (goto-char (point-min))
	   (vtable-objects (vtable-current-table))))
	 (urls (cl-loop for (_ url) in objects
			when (and (and (bang--media-p url)
				       (not (string-match "[.]mp4\\'" url)))
				  (string-match-p
				   (format-time-string "/uploads/%Y/%m/")
				   url)
				  (not (gethash url bang--shown-media)))
			collect url)))
    (unless urls
      (error "No URLs today"))
    (dolist (url urls)
      (setf (gethash url bang--shown-media) t))
    ;; This code doesn't really make sense in general, so it should be
    ;; factored out.
    (let ((browse-url-browser-function browse-url-secondary-browser-function))
      (browse-url (pop urls))
      (sleep-for 2)
      (when urls
	(let ((browse-url-firefox-program "/usr/local/bin/firefox/firefox"))
	  (dolist (url urls)
	    (browse-url url)))))))

(defun bang--view-clicks (domain)
  (switch-to-buffer "*Clicks*")
  (bang-clicks-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Clicks"))
     :objects (bang-sel "select count(click), click from clicks where time > ? and domain = ? group by click order by count(click) desc"
			(bang--now) domain)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-clicks-mode-map)))

(defun bang--view-total-views (_ &optional date)
  (unless date
    (switch-to-buffer "*Total Bang*"))
  (let ((inhibit-read-only t)
	(from (bang--now))
	(to (bang--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Blog" :max-width 20)
		(:name "Posts & Pages"))
     :objects (bang-sel "select count(page), blog, title, page from views where time > ? and time <= ? group by page order by count(page) desc, id"
			from to)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Posts & Pages")
	   (buttonize (bang--pretty-url (elt elem column))
		      #'bang--browse (elt elem 2) (elt elem 2))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--view-total-referrers (_ &optional date)
  (unless date
    (switch-to-buffer "*Total Bang*"))
  (let ((inhibit-read-only t)
	(from (bang--now))
	(to (bang--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Referrers"))
     :objects (bang--transform-referrers
	       (bang-sel "select count(referrer), referrer from referrers where time > ? and time <= ? group by referrer order by count(referrer) desc"
			 from to))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Referrers")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--view-total-clicks (_ &optional date)
  (unless date
    (switch-to-buffer "*Total Bang*"))
  (let ((inhibit-read-only t)
	(from (bang--now))
	(to (bang--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Clicks"))
     :objects (bang-sel "select count(distinct click), click from clicks where time > ? and time <= ? group by click order by count(click) desc"
			from to)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--view-total-countries (_ &optional date)
  (unless date
    (switch-to-buffer "*Total Bang*"))
  (let ((inhibit-read-only t)
	(from (bang--now))
	(to (bang--future)))
    (if date
	(setq from (concat date " 00:00:00")
	      to (concat date " 23:59:59"))
      (setq truncate-lines t)
      (erase-buffer))
    (make-vtable
     :use-header-line (not date)
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Countries"))
     :objects (bang-sel "select count(country), name, code from views, countries where time > ? and time <= ? and views.country = countries.code group by country order by count(country) desc"
			from to)
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Countries")
	   (bang--countrify (elt elem 2) (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--transform-referrers (referrers &optional summarize)
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for (count url) in referrers
	     for trans = (bang--transform-referrer url summarize)
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
					 (bang--view-referrer-details urls))
				       urls))
			      urls)
			     result)))
		    (t
		     (push (list length referrer urls) result)))))
	       table)
      (let ((list (nreverse (sort result #'car-less-than-car))))
	(if summarize
	    (seq-take list bang-entries)
	  list)))))

(defun bang--transform-referrer (url &optional summarize)
  (cond
   ((string-match-p "[.]bing[.][a-z]+/\\'" url)
    (if summarize "Search" "Bing"))
   ((string-match-p "[.]google[.][.a-z]+[/]?\\'" url)
    (if summarize "Search" "Google"))
   ((string-match-p "[.]reddit[.]com/\\'" url)
    "Reddit")
   ((string-match-p "[.]?bsky[.]app/\\'" url)
    "Bluesky")
   ((string-match-p "search[.]brave[.]com/\\'" url)
    (if summarize "Search" "Brave"))
   ((string-match-p "\\b[.]?baidu[.]com/" url)
    (if summarize "Search" "Baidu"))
   ((string-match-p "[a-z]+[.]wikipedia[.]org/\\'" url)
    "Wikipedia")
   ((string-match-p "search[.]yahoo[.]com/\\'" url)
    (if summarize "Search" "Brave"))
   ((string-match-p "\\bduckduckgo[.]com/\\'" url)
    (if summarize "Search" "DuckDuckGo"))
   ((string-match-p "\\byandex[.]ru/" url)
    (if summarize "Search" "Yandex"))
   ((string-match-p "\\b[.]qwant[.]com/\\'" url)
    (if summarize "Search" "Qwant"))
   ((string-match-p "\\b[.]?kagi[.]com/\\'" url)
    (if summarize "Search" "Kagi"))
   ((string-match-p "\\b[.]ecosia[.]org/\\'" url)
    (if summarize "Search" "Ecosia"))
   ((and summarize (string-match-p "\\byandex[.]ru/" url))
    "Search")
   ((and summarize (string-match-p "\\ampproject[.]org/" url))
    "Amp")
   ((equal (bang--host url) "t.co")
    "Twitter")
   ((equal (bang--get-domain (bang--host url)) "facebook.com")
    "Facebook")
   ((and summarize (member (bang--host url) bang-blogs))
    "Interblog")
   (summarize
    (concat "-" (bang--get-domain (bang--host url))))
   (t
    url)))

;; Plots.

(defun bang--plot-blogs-today ()
  (let ((data 
	 (bang-sel "select blog, count(blog), count(distinct ip) from views where time > ? group by blog order by blog"
		   (bang--now))))
    (insert-image
     (svg-image
      (eplot-make-plot
       `((Format horizontal-bar-chart)
	 (Color vary)
	 (Mode dark)
	 (Layout compact)
	 (Font ,bang-font)
	 (Margin-Left 10)
	 (Horizontal-Label-Left 20)
	 (Horizontal-Label-Font-Size 18)
	 (Height 300)
	 (Width 250))
       (append
	'((Bar-Max-Width: 40))
	(cl-loop for (blog views _visitors) in data
		 collect (list views "# Label: " blog)))
       (append
	'((Bar-Max-Width: 20)
	  (Color: "#004000"))
	(cl-loop for (blog _views visitors) in data
		 collect (list visitors "# Label: " blog)))))
     "*")))

(defun bang--plot-history ()
  (let ((data (bang-sel "select date, sum(views), sum(visitors) from history group by date order by date limit 14"))
	(today (car (bang-sel "select count(*), count(distinct ip) from views where time > ?"
			      (bang--now))))
	(current (car (bang-sel "select count(*), count(distinct ip) from views where time > ?"
				(format-time-string "%Y-%m-%d 00:00:00")))))
    (insert-image
     (svg-image
      (eplot-make-plot
       `((Mode dark)
	 (Layout compact)
	 (Font ,bang-font)
	 (Height 300)
	 (Width 550)
	 (Format bar-chart))
       (append
	'((Bar-Max-Width: 20)
	  (Color: "#004000"))
	(cl-loop for (date _views visitors) in data
		 collect (list visitors "# Label: " (substring date 8)))
	(list (list (cadr current) "# Label: " (format-time-string "%d")))
	(list (list (cadr today) "# Label: 24h")))
       (append
	'((Bar-Max-Width: 40)
	  (Color: "#008000"))
	(cl-loop for (date views _visitors) in data
		 collect (list views "# Label: " (substring date 8)))
	(list (list (car current) "# Label: " (format-time-string "%d")))
	(list (list (car today) "# Label: 24h")))))
     "*")))

(provide 'bang)

;;; bang.el ends here

;; Todo:
