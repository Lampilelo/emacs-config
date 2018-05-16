(defun my-lyrics ()
  (interactive)
  "Gets lyrics for a song playing in MOC player. Requires lyrics package."
  ;; check if mocp is running
  (if (file-exists-p (concat (getenv "HOME")"/.moc/pid"))
      ;; check if mocp is playing
      (if (string-equal (shell-command-to-string "mocp -Q %file") "\n")
	  (message "MOC is not playing anything.")
	(let* ((artist (string-remove-suffix "\n" (shell-command-to-string "mocp -Q %artist")))
	       (song (string-remove-suffix "\n" (shell-command-to-string "mocp -Q %song"))))
	  ;; if both artist and song are not empty
	  (if (not (or (string-empty-p artist)
		       (string-empty-p song)))
	      (lyrics artist song)	;show lyrics
	    ;; else
	    (message "Could not determine song title or artist.\nPlease check if metadata is present in current playing file (ID3 tags for mp3, Vorbis comments for flac, etc.)"))))
    (message "MOC Player is not running.")))

;; key for refreshing lyrics if there's playing a new song
(define-key lyrics-show-mode-map (kbd "g") 'my-lyrics)
(define-key lyrics-show-mode-map (kbd "n") 'scroll-up-line)
(define-key lyrics-show-mode-map (kbd "p") 'scroll-down-line)
(define-key lyrics-show-mode-map (kbd "G") '(lambda () (interactive)
					      (setq current-prefix-arg "t")
					      (call-interactively 'lyrics)))
