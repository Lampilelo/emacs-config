;;; lrc-get.el --- Get lrc lyrics file from the internet -*- lexical-binding: t -*-

;; Copyright (C) 2019 Jakub Wojciech

;; Author: Jakub Wojciech <jakub-w@riseup.net>
;; Maintainer: Jakub Wojciech
;; Version: 0.1
;; Created: 19.03.2019
;; Keywords: convenience, lyrics

(defvar lrc-search-url-template
  "https://syair.info/search?artist=%s&title=%s")
(defvar lrc-lyrics-url-template
  "https://syair.info%s")

(defun lrc--get-search-url (artist title)
  (url-encode-url
   (format lrc-search-url-template artist title)))

;; TODO: the buffer could be created inside of this function
(defun lrc--get-lyrics-url-from-url-retrieve-buffer (buffer artist title)
  "Return url when found, nil otherwise."
  (let ((html-tree
	 (with-current-buffer buffer
	   (goto-char 1)
	   (unless (looking-at-p "HTTP/")
	     (error "Malformed lrc response."))
	   (libxml-parse-html-region (point-min) (point-max)))))
    ;; iterate over lyric links and return the first matching an artist and
    ;; a title
    (cl-do* ((lyric-links (dom-by-tag html-tree 'article) (cdr lyric-links))
	     (link (dom-by-tag (car lyric-links) 'a)
		   (dom-by-tag (car lyric-links) 'a)))
	((or (null lyric-links)
	     (string-match-p (format "%s.*%s" artist title)
			     (dom-text link)))
	 (if (null lyric-links)
	     nil
	   (format lrc-lyrics-url-template (dom-attr link 'href)))))))

(defun lrc--get-lyrics (url-retrieve-buffer)
  (with-current-buffer url-retrieve-buffer
    (goto-char (point-min))
    (save-restriction
      (narrow-to-region (search-forward "<div class=\"entry\">")
  			(- (search-forward "</div>") 6))
      (html2text-remove-tags '("br" "div" "script" "!"))
      (whitespace-cleanup)
      (buffer-string))))

;;;###autoload
(defun lrc-get-lyrics (artist title)
  "Return the string of timestamped lyrics for a song specified by ARTIST
and TITLE."
  (lrc--get-lyrics
   (url-retrieve-synchronously
    (lrc--get-lyrics-url-from-url-retrieve-buffer
     (url-retrieve-synchronously (lrc--get-search-url artist title))
     artist title))))

(provide 'lrc-get)
