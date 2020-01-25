(package-initialize)
(load "~/.emacs.d/my-lyrics.el" t t)

(let ((result (my-lyrics)))
  (cond
   ((windowp result)
    (with-current-buffer (window-buffer result)
      (message (buffer-string))))
   ((bufferp result)
    (while (not (get-buffer "*Lyrics*"))
      (sleep-for 0.25))
    (with-current-buffer (get-buffer "*Lyrics*")
      (message "\n%s" (buffer-string))))))
