(package-initialize)
(load "~/.emacs.d/my-lyrics.el" t t)

(let ((result (my-lyrics)))
  (cond
   ((windowp result)
    (with-current-buffer (window-buffer result)
      (princ (format "%s\n" (buffer-string)))))
   ((bufferp result)
    (while (not (get-buffer "*Lyrics*"))
      (sleep-for 0.25))
    (with-current-buffer (get-buffer "*Lyrics*")
      (princ (format "\n%s\n" (buffer-string)))))))
