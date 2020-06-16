;; -*- lexical-binding: t -*-

(require 'elfeed)
(require 'subr-x)
(require 'cl)

(let* ((sentinel
	(lambda (process event)
	  (unless (process-live-p process)
	    (with-current-buffer (process-buffer process)
	      (if (= (process-exit-status process) 0)
		  (kill-buffer (current-buffer))
		(ansi-color-apply-on-region (point-min) (point-max))
		(read-only-mode 1)
		(pop-to-buffer (current-buffer))))))))
  (defun my-start-process-show-errors (name program &rest program-args)
    (let* ((buffer (generate-new-buffer (format " *%s*" name))))
      (make-process :name name
		    :buffer buffer
		    :command (cons program program-args)
		    :sentinel sentinel))))

(defun my-elfeed-mpv-play (url &optional no-video)
  "Play URL in MPV player.

NO-VIDEO should be set to non-nil to start MPV with --no-video option in
a terminal application.

When used interactively URL is the url at point.
NO-VIDEO can be set with a prefix argument \\[universal-argument]."
  (interactive (list (thing-at-point 'url) current-prefix-arg))
  (cl-check-type url string "Expected URL as string")
  (if no-video
      (my-start-process-show-errors
       "mpv"
       "alacritty"
       "-e" ;(concat "mpv --no-video '" url "'")
       "mpv" "--no-video" url)
    (my-start-process-show-errors
     "mpv" "mpv"
     (format "--ytdl-raw-options=%s%s%s"
	     "format=bestvideo[height<="
	     (or (display-pixel-height) 1080)
	     "]+bestaudio")
     url)))

(defun my-elfeed--bitchute-video-url (url)
  "Return the url of an mp4 file given bitchute video's URL."
  (when (string-match "/embed/" url)
    (setq url (replace-match "/video/" nil nil url)))
  ;; TODO: Try 3 times for a response lower than 400
  (with-current-buffer (url-retrieve-synchronously url)
    (let ((dom (libxml-parse-html-region (point-min) (point-max))))
      (dom-attr (car (dom-children (dom-by-id dom "player"))) 'src))))

(defun my-elfeed-open-link ()
  (interactive)
  (let ((entries (elfeed-search-selected)))
    (cl-loop
     for entry in entries
     do (elfeed-untag entry 'unread)
     when (elfeed-entry-link entry)
     do (let* ((enclosures (elfeed-entry-enclosures entry)))
	  ;; If there is an audio file in enclosures, open it
	  ;; with --no-video
	  (if (and enclosures
		   (string-match-p "^audio/"
				   (nth 1 (car enclosures))))
	      (my-elfeed-mpv-play (caar enclosures) 'no-video)
	    ;; else open in mpv proper
	    (cond
	     ((string-match-p "bitchute.com/\\(embed\\|video\\)" it)
	      (my-elfeed-mpv-play (my-elfeed--bitchute-video-url it)))
	     (t (my-elfeed-mpv-play it))))
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


;; TODO: Create an advice for the function in elfeed that reads elfeed-feeds.
;;       my-elfeed-feeds should store structs with name, url, category and
;;       enabled keywords (or maybe plists with these keywords).
