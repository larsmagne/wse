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

(defface bang
  '((t :family "Futura"))
  "The face to use in bang buffers.")

(defvar bang-blogs nil
  "A list of blogs to collect statistics from.
This should be a list of names (like \"foo.org\" and not URLs.")

(defvar bang--db nil)

(defun bang--poll-blogs (&optional callback)
  (let ((blogs bang-blogs)
	(data nil)
	func)
    (setq func
	  (lambda ()
	    (let* ((blog (pop blogs))
		   (id (or (caar
			    (sqlite-select
			     bang--db "select last_id from blogs where blog = ?"
			     (list blog)))
			   0))
		   (url-request-method "POST")
		   (url-request-extra-headers
		    '(("Content-Type" . "application/x-www-form-urlencoded")
		      ("Charset" . "UTF-8")))
		   (url-request-data
		    (mm-url-encode-www-form-urlencoded
		     `(("from_id" . ,(format "%d" id))
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
		     (bang--update-data data callback))))))))
    (funcall func)))      

(defvar bang--filling-country nil)

(defun bang--update-data (data &optional callback)
  (setq d2 data)
  (cl-loop for (blog . elems) in data
	   do (cl-loop for elem across (gethash "data" elems)
		       for (id time click page referrer ip user-agent title) =
		       (cl-coerce elem 'list)
		       unless (bang--bot-p user-agent)
		       do
		       (bang--insert-data blog time click page referrer ip
					  user-agent title)
		       (bang--update-id blog id)))
  (unless bang--filling-country
    (bang--fill-country))
  (when callback
    (funcall callback)))

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
    (unless (sqlite-select bang--db "select * from country_counter")
      (sqlite-execute bang--db "insert into country_counter values (0)"))
    (sqlite-execute bang--db "create table if not exists views (id integer primary key, blog text, date date, time datetime, page text, ip text, user_agent text, title text, country text)")
    (sqlite-execute bang--db "create table if not exists referrers (id integer primary key, blog text, time datetime, referrer text, page text)")
    (sqlite-execute bang--db "create table if not exists clicks (id integer primary key, blog text, time datetime, click text, domain text, page text)")
    (sqlite-execute bang--db "create table if not exists history (id integer primary key, blog text, date date, views integer, visitors integer, clicks integer, referrers integer)")
    (sqlite-execute bang--db "create table if not exists countries (code text primary key, name text)")))

(defun bang--host (url)
  (url-host (url-generic-parse-url url)))

(defun bang--url-p (string)
  (and (and (stringp string))
       (not (zerop (length string)))
       (string-match-p "\\`[a-z]+:" string)))

(defun bang--media-p (click)
  (string-match "[.]\\(mp4\\|png\\|jpg\\|jpeg\\|webp\\|webp\\)\\'" click))

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
	  (sqlite-execute
	   bang--db
	   "insert into clicks(blog, time, click, domain, page) values(?, ?, ?, ?, ?)"
	   (list blog time click (bang--host click) page)))
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
  :parent button-map
  "g" #'bang-revert)

(defun bang-revert ()
  "Update the current buffer."
  (interactive)
  (message "Updating...")
  (bang--poll-blogs #'bang))

(defun bang ()
  "Display Wordpress statistics."
  (interactive)
  (pop-to-buffer "*Bang*")
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
    (goto-char (point-min))
    (insert "\n")
    (goto-char (point-min))
    (bang--plot-history)
    (bang--plot-blogs-today)
    (insert "\n")
    ))

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

(defun bang--get-page-table-data ()
  (let* ((time (bang--now))
	 (pages
	  (sqlite-select
	   bang--db
	   "select count(page), title, page from views where time > ? group by page order by count(page) desc limit 10"
	   (list time)))
	 (referrers
	  (bang--transform-referrers
	   (sqlite-select
	    bang--db
	    "select count(referrer), referrer from referrers where time > ? group by referrer order by count(referrer) desc"
	    (list time))
	   t)))
    (nconc
     (cl-loop for i from 0 upto 9
	      for page = (elt pages i)
	      collect
	      (append (if page
			  (list (nth 0 page)
				(buttonize
				 (if (bang--url-p (nth 1 page))
				     (bang--pretty-url (nth 1 page))
				   (nth 1 page))
				 #'bang--browse (elt page 2)
				 (elt page 2)))
			(list "" ""))
		      (or (elt referrers i) (list "" ""))))
     (list
      (list
       (caar (sqlite-select bang--db "select count(*) from views where time > ?"
			    (list time)))
       (buttonize "Total Views" #'bang--view-total-views)
       (caar (sqlite-select bang--db "select count(*) from referrers where time > ?"
			    (list time)))
       (buttonize "Total Referrers" #'bang--view-total-referrers))))))

(defun bang--now ()
  (bang--time (- (time-convert (current-time) 'integer)
		 (* 60 60 24))))

(defun bang--get-click-table-data ()
  (let* ((time (bang--now))
	 (clicks
	  (sqlite-select
	   bang--db
	   "select count(domain), domain, count(distinct click), click from clicks where time > ? group by domain order by count(domain) desc limit 10"
	   (list time)))
	 (countries
	  (sqlite-select
	   bang--db
	   "select count(country), name, code from views, countries where time > ? and views.country = countries.code group by country order by count(country) desc limit 10"
	   (list time))))
    (nconc
     (cl-loop for i from 0 upto 9
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
       (caar (sqlite-select bang--db "select count(*) from clicks where time > ?"
			    (list time)))
       (buttonize "Total Clicks" #'bang--view-total-clicks)
       (caar (sqlite-select bang--db "select count(distinct country) from views where time > ?"
			    (list time)))
       (buttonize "Total Countries" #'bang--view-total-countries))))))

(defun bang--view-clicks (domain)
  (switch-to-buffer "*Clicks*")
  (special-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Clicks"))
     :objects (sqlite-select
	       bang--db
	       "select count(click), click from clicks where time > ? and domain = ? group by click order by count(click) desc"
	       (list (bang--now) domain))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--view-total-views (_)
  (switch-to-buffer "*Total Bang*")
  (special-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Posts & Pages"))
     :objects (sqlite-select
	       bang--db
	       "select count(page), title, page from views where time > ? group by page order by count(page) desc, id"
	       (list (bang--now)))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Posts & Pages")
	   (buttonize (bang--pretty-url (elt elem column))
		      #'bang--browse (elt elem 2) (elt elem 2))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--browse (url)
  (let ((browse-url-browser-function
	 (if (bang--media-p url)
	     browse-url-browser-function
	   browse-url-secondary-browser-function)))
    (browse-url url)))

(defun bang--view-total-referrers (_)
  (switch-to-buffer "*Total Bang*")
  (special-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Referrers"))
     :objects (bang--transform-referrers
	       (sqlite-select
		bang--db
		"select count(referrer), referrer from referrers where time > ? group by referrer order by count(referrer) desc"
		(list (bang--now))))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Referrers")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--view-total-clicks (_)
  (switch-to-buffer "*Total Bang*")
  (special-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Clicks"))
     :objects (sqlite-select
	       bang--db
	       "select count(distinct click), click from clicks where time > ? group by click order by count(click) desc"
	       (list (bang--now)))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Clicks")
	   (bang--possibly-buttonize (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--view-total-countries (_)
  (switch-to-buffer "*Total Bang*")
  (special-mode)
  (let ((inhibit-read-only t))
    (setq truncate-lines t)
    (erase-buffer)
    (make-vtable
     :face 'bang
     :columns '((:name "" :align 'right)
		(:name "Countries"))
     :objects (sqlite-select
	       bang--db
	       "select count(country), name, code from views, countries where time > ? and views.country = countries.code group by country order by count(country) desc"
	       (list (bang--now)))
     :getter
     (lambda (elem column vtable)
       (if (equal (vtable-column vtable column) "Countries")
	   (bang--countrify (elt elem 2) (elt elem column))
	 (elt elem column)))
     :keymap bang-mode-map)))

(defun bang--transform-referrers (referrers &optional summarize)
  (let ((table (make-hash-table :test #'equal)))
    (cl-loop for (count url) in referrers
	     do (cl-incf (gethash (bang--transform-referrer url summarize)
				  table 0)
			 count))
    (let ((result nil))
      (maphash (lambda (referrer count)
		 (push (list count referrer) result))
	       table)
      (seq-take (nreverse (sort result #'car-less-than-car)) 10))))

(defun bang--transform-referrer (url &optional summarize)
  (cond
   ((string-match-p "[.]bing[.][a-z]+/\\'" url)
    (if summarize "Search" "Bing"))
   ((string-match-p "[.]google[.][.a-z]+[/]?\\'" url)
    (if summarize "Search" "Google"))
   ((string-match-p "[.]reddit[.]com/\\'" url)
    (if summarize "Search" "Reddit"))
   ((string-match-p "search[.]brave[.]com/\\'" url)
    (if summarize "Search" "Brave"))
   ((string-match-p "\\bduckduckgo[.]com/\\'" url)
    (if summarize "Search" "DuckDuckGo"))
   ((string-match-p "\\byandex[.]ru/\\'" url)
    (if summarize "Search" "Yandex"))
   ((and summarize (string-match-p "\\byandex[.]ru/" url))
    "Search")
   ((and summarize (string-match-p "\\ampproject[.]org/" url))
    "Amp")
   ((equal (bang--host url) "t.co")
    "Twitter")
   ((and summarize (member (bang--host url) bang-blogs))
    "Interblog")
   (t
    url)))

(defun bang--time (time)
  (format-time-string "%Y-%m-%d %H:%M:%S" time))

(defun bang--fill-country ()
  (setq bang--filling-country t)
  (let ((id (or (caar (sqlite-select bang--db "select id from country_counter"))
		0))
	func)
    (setq func
	  (lambda ()
	    (let ((next (caar
			 (sqlite-select
			  bang--db "select min(id) from views where id > ?"
			  (list id)))))
	      (if (not next)
		  (setq bang--filling-country nil)
		(url-retrieve
		 (format "http://ip-api.com/json/%s"
			 (caar (sqlite-select bang--db
					      "select ip from views where id = ?"
					      (list next))))
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
		     (sqlite-execute bang--db
				     "update views set country = ? where id = ?"
				     (list country-code next))
		     (sqlite-execute bang--db
				     "update country_counter set id = ?"
				     (list next))
		     (when (and country-name
				(not (sqlite-select
				      bang--db "select * from countries where code = ?"
				      (list country-code))))
		       (sqlite-execute bang--db "insert into countries(code, name) values (?, ?)"
				       (list country-code country-name)))
		     (setq id next)
		     ;; The API is rate limited at 45 per minute, so
		     ;; poll max 30 times per minute.
		     (run-at-time 2 nil func)))
		 nil t)))))
    (funcall func)))

(defun bang--plot-blogs-today ()
  (let ((data 
	 (sqlite-select bang--db "select blog, count(blog), count(distinct ip) from views where time > ? group by blog order by blog"
			(list (bang--now)))))
    (insert-image
     (svg-image
      (eplot-make-plot
       '((Format horizontal-bar-chart)
	 (Color vary)
	 (Mode dark)
	 (Layout compact)
	 (Font Futura)
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

(defun bang--summarise-history ()
  (dolist (blog bang-blogs)
    (cl-loop with max-date = (caar (sqlite-select bang--db "select date from views where blog = ? order by id desc limit 1"
						  (list blog)))
	     for (date views visitors) in
	     (sqlite-select bang--db "select date, count(date), count(distinct ip) from views where date < ? and blog = ? group by date order by date"
			    (list max-date blog))
	     unless (sqlite-select bang--db "select date from history where blog = ? and date = ?"
				   (list blog date))
	     do (sqlite-execute
		 bang--db "insert into history(blog, date, views, visitors, clicks, referrers) values (?, ?, ?, ?, ?, ?)"
		 (list blog date views visitors
		       (caar (sqlite-select
			      bang--db "select count(*) from clicks where blog = ? and time between ? and ?"
			      (list blog (concat date " 00:00:00")
				    (concat date " 23:59:59"))))
		       (caar (sqlite-select
			      bang--db "select count(*) from referrers where blog = ? and time between ? and ?"
			      (list blog (concat date " 00:00:00")
				    (concat date " 23:59:59")))))))))

(defun bang--plot-history ()
  (let ((data (sqlite-select bang--db "select date, sum(views), sum(visitors) from history group by date order by date limit 14"))
	(today (car (sqlite-select bang--db "select count(*), count(distinct ip) from views where time > ?"
				   (list (bang--now))))))
    (insert-image
     (svg-image
      (eplot-make-plot
       '((Mode dark)
	 (Layout compact)
	 (Font Futura)
	 (Height 300)
	 (Width 550)
	 (Format bar-chart))
       (append
	'((Bar-Max-Width: 20)
	  (Color: "#004000"))
	(cl-loop for (date _views visitors) in data
		 collect (list visitors "# Label: " (substring date 8)))
	(list (list (cadr today) "# Label: today")))
       (append
	'((Bar-Max-Width: 40)
	  (Color: "#008000"))
	(cl-loop for (date views _visitors) in data
		 collect (list views "# Label: " (substring date 8)))
	(list (list (car today) "# Label: today")))))
     "*")))

(provide 'bang)

;;; bang.el ends here

;; Todo:
;; Group referrers by domain
;; Summarise history per day
;; Instrument mp4
;; Instrument lyte
;; Allow choosing a historic date
;; Don't record own clicks
;; Command to see mode data about viewers of post
;; Command to see where Referrers went
;; Output comments from data.php
;; Spread eplot bars totally evenly
;; bang.js is collecting too many titles, and wrong title for main page?
;; Display update time in *Bang*
;; Command to open today's media links
