(defun my-tablify ()
    (interactive)
  (goto-char (region-end))
  (let* ((end-marker (copy-marker (point-marker))))
    (goto-char (region-beginning))
    (goto-char (line-beginning-position))
    (insert "|")
    (forward-word)
    (insert "|")
    (org-delete-char 1)
    (goto-char (line-end-position))
    (insert "|")
    (forward-line)
    (insert"|-+-|")
    (newline)

    (dotimes (counter (count-lines (point-marker) end-marker))
      (insert (format "|%d|" (+ counter 1)))
      (goto-char (line-end-position))
      (insert "|")
      (forward-line))
    (org-table-align)
))


(defun M-x-other-window ()
      "Call M-x in the other window."
      (interactive)
      (save-selected-window
        (other-window 1)
        (execute-extended-command nil)))


;; loop example
(defun temp/find-root () (interactive)
       (message "%s"
		(loop for (key . value) in my/c++-build-systems-alist
		      for var = (vc-find-root buffer-file-name key)
		      until (not (eq var nil))
		      finally return var)))


;; IN-PROGRESS

