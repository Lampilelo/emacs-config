;; -*- lexical-binding: t -*-

(require 'elfeed)
(require 'subr-x)
(require 'cl)

(defun my-elfeed-open-link ()
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop for entry in entries
	     do (elfeed-untag entry 'unread)
	     when (elfeed-entry-link entry)
	     do (let* ((enclosures (elfeed-entry-enclosures entry)))
		  ;; If there is an audio file in enclosures, open it
		  ;; with --no-video
		  (if (and enclosures
			     (string-match-p "^audio/"
					     (nth 1 (car enclosures))))
		      (start-process "mpv" nil
				     "i3-sensible-terminal"
				     "-e" (concat "mpv --no-video '"
						  (caar enclosures)
						  "'"))
		    ;; else open in mpv proper
		    (start-process "mpv" nil "mpv"
				   (format "--ytdl-raw-options=%s%s%s"
					   "format=bestvideo[height<="
					   (or (display-pixel-height) 1080)
					   "]+bestaudio")
				   it))
		  (elfeed-search-show-entry entry)))
    (mapc #'elfeed-search-update-entry entries)))

(define-key elfeed-search-mode-map (kbd "v") #'my-elfeed-open-link)

(defun my-elfeed-update-feed-local (file)
  "Like `elfeed-update-feed' but uses local FILE, not url.

Designed to use as advice to `elfeed-update-feed' with `:before-until' combinator."
  (if (string-prefix-p "file:" file)
      (let ((file-path (string-trim-left file "file:")))
	(if (not (file-exists-p file-path))
	    (elfeed-handle-parse-error file "File doesn't exist.")
	  (progn
	    (unless elfeed--inhibit-update-init-hooks
	      (run-hooks 'elfeed-update-hooks))
	    (condition-case error
		(let ((feed (elfeed-db-get-feed file)))
		  (with-temp-buffer
		    (insert-file-contents file-path)
		    (let* ((xml (elfeed-xml-parse-region (point-min)
							 (point-max)))
			   (entries
			    (cl-case(elfeed-feed-type xml)
			      (:atom (elfeed-entries-from-atom file xml))
			      (:rss (elfeed-entries-from-rss file xml))
			      (:rss1.0 (elfeed-entries-from-rss1.0 file xml))
			      (otherwise
			       (error (elfeed-handle-parse-error
				       file
				       "Unknown feed type."))))))
		      (elfeed-db-add entries))))
	      (error (elfeed-handle-parse-error file error)))))
	(run-hook-with-args 'elfeed-update-hooks file))
    ;; return nil if not local file so that elfeed-update-feed can take over
    nil))
(advice-add 'elfeed-update-feed :before-until #'my-elfeed-update-feed-local)


(defvar my-elfeed-feeds nil
  "((category1
  (\"name a\" . \"https://link.a\")
  (\"name b\" . \"https://link.b\"))
 (category2
  (\"name c\" . \"https://link.c\")
  (\"name d\" . \"https://link.d\")))")

(defvar my-elfeed-base-links
  '((bitchute . "https://www.bitchute.com/feeds/rss/channel/")
    (youtube . "https://www.youtube.com/feeds/videos.xml?channel_id=")))

(defun my-elfeed-add-feed (feed-id name service category)
  (let ((current-category
	 (progn (unless (assoc category my-elfeed-feeds)
		  (add-to-list 'my-elfeed-feeds (list category)))
		(assoc category my-elfeed-feeds))))
    (unless (assoc name (cdr current-category))
      (setcdr current-category
	      (cons
	       (cons name
		     (concat (alist-get service my-elfeed-base-links)
			     feed-id))
	       (cdr current-category)))))
  my-elfeed-feeds)

(defun my-elfeed-add-feeds (feeds service category)
  "FEEDS is a list of (name . feed-id) pairs.
SERVICE is a symbol from `my-elfeed-base-links'.
CATEGORY is a symbol."
  (dolist (feed feeds)
    (my-elfeed-add-feed (cdr feed) (car feed) service category)))

(defun my-elfeed-enable-category (category)
  "Enable feeds from the certain CATEGORY in `my-elfeed-feeds'."
  (dolist (feed (alist-get category my-elfeed-feeds))
    (cl-pushnew (list (cdr feed) category) elfeed-feeds :test 'equal)))
(defun my-elfeed-enable-categories (&rest categories)
  "Enable feeds from the certain CATEGORIES in `my-elfeed-feeds'."
  (dolist (category categories)
    (my-elfeed-enable-category category)))

;; FIXME: it expects feeds to have only one category
(defun my-elfeed-disable-category (category)
  "Disable feeds from the certain CATEGORY in `my-elfeed-feeds'."
  (dolist (feed (alist-get category my-elfeed-feeds))
    (setq elfeed-feeds (delete (list (cdr feed) category) elfeed-feeds))))
(defun my-elfeed-disable-categories (&rest categories)
  "Disable feeds from the certain CATEGORIES in `my-elfeed-feeds'."
  (dolist (category categories)
    (my-elfeed-disable-category category)))

(defun my-elfeed-disable-by-name (&rest names)
  "Disable singular feeds."
  (dolist (category my-elfeed-feeds)
    (dolist (feed (cdr category))
      (when (member (car feed) names)
	(setq elfeed-feeds (delete (list (cdr feed) (car category))
				   elfeed-feeds))))))

(defun my-elfeed-get-feed-name (url)
  "Return the name of a feed in `my-elfeed-feeds' that matches the URL."
  (catch 'return
    (seq-do (lambda (category)
	      (seq-do (lambda (feed)
			(when (string-equal (cdr feed) url)
			  (throw 'return (car feed))))
		      (cdr category)))
	    my-elfeed-feeds)
    nil))

(defun my-elfeed-get-feed-url (name)
  "Return the url of a feed in `my-elfeed-feeds' that matches the NAME."
  (catch 'return
    (seq-do (lambda (category)
	      (seq-do (lambda (feed)
			(when (string-equal (car feed) name)
			  (throw 'return (cdr feed))))
		      (cdr category)))
	    my-elfeed-feeds)
    nil))

