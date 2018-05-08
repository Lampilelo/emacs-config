#!/usr/bin/env sh

emacs --no-init-file \
      --no-window-system \
      --eval "(progn (menu-bar-mode -1)
      	             (calc)
	             (let* ((buffer (get-buffer \"*scratch\")))
	               (delete-windows-on buffer)
		       (kill-buffer buffer))
		     (define-key calc-mode-map (kbd \"q\") 'save-buffers-kill-terminal))"
