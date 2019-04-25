(require 'subr-x)
(require 'lyrics)

;; TODO: save lyrics to a song file with mid3v2 (check ~/Notes/mid3v2.org)
(defun my-lyrics ()
  "Gets lyrics for a song playing in MOC player. Requires lyrics package."
  (interactive)
  ;; check if mocp is running
  (let ((info (split-string (shell-command-to-string
			     "mocp -Q '%file\t%artist\t%song'")
			    "\t" nil "\n")))
    (cond
     ((not (file-exists-p (substitute-env-in-file-name "$HOME/.moc/pid")))
      (message "MOC Player is not running."))
     ((string-empty-p (car info))
      (message "MOC is not playing anything."))
     ((or (string-empty-p (cadr info))
	  (string-empty-p (caddr info)))
      (message "Could not determine song title or artist.\nPlease check if metadata is present in current playing file (ID3 tags for mp3, Vorbis comments for flac, etc.)"))
     (t (lyrics (cadr info) (caddr info))))))

;; key for refreshing lyrics if there's playing a new song
(define-key lyrics-show-mode-map (kbd "g") #'my-lyrics)
(define-key lyrics-show-mode-map (kbd "n") #'scroll-up-line)
(define-key lyrics-show-mode-map (kbd "p") #'scroll-down-line)
(define-key lyrics-show-mode-map (kbd "G")
  #'(lambda () (interactive)
      (setq current-prefix-arg "t")
      (call-interactively 'lyrics)))
