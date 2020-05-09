(defun cppedit-transpose-sexps (arg)
  (interactive "*p")
  (if (or (looking-at-p "[ \t\n]*,")
	  (and (ignore-errors (looking-back ",[ \t\n]*"
					    (or (cadr (syntax-ppss))
						(error ""))))
	       (search-backward ",")))
      ;; we know we're before a comma
      (cppedit-transpose-args)
    (transpose-sexps arg)))


(defvar cppedit-sexp-delimiters '("," "\\bor\\b" "\\band\\b" "[!<>=]?="))
(defvar cppedit-whitespace-regex "[ \t\n]*")

;; TODO: Skip the separator if the depth in parens is higher than paren-depth
(defun cppedit--next-separator (&optional paren-depth)
  (unless paren-depth (setq paren-depth (car (syntax-ppss))))
  (let ((result (or (ignore-errors
		      (re-search-forward
		       (concat cppedit-whitespace-regex "\\(?:"
			       (string-join cppedit-sexp-delimiters "\\|")
			       "\\)" cppedit-whitespace-regex))
		      (cons (match-beginning 0) (match-end 0)))
		    (cons (point-max) (point-max)))))
    (if (and result (> (car (syntax-ppss)) paren-depth))
	(cppedit--next-separator paren-depth)
      result)))

;; FIXME: If there's a whitespace at the beginning or the end of the narrowed
;;        region, the whitespace is added to the first and the last element.
;;        Maybe we should get rid of them when narrowing.
(defun cppedit-transpose-args ()
  (interactive)
  (save-restriction
    (let ((oldpoint (point))
	  (syntax (syntax-ppss)))
      (goto-char (nth 1 syntax))
      (narrow-to-region (1+ (point)) (1- (scan-sexps (point) 1)))
      (goto-char (point-min))
      (let ((run 't)
	    (last-last-sep (cons (point-min) (point-min)))
	    (last-sep (cons (point-min) (point-min)))
	    sep
	    point-relative
	    ret)
	(while run
	  (setq sep (cppedit--next-separator))
	  (cond
	   ((not sep) (setq run nil))
	   ((and (>= oldpoint (car last-sep))
		 (<= oldpoint (cdr last-sep)))
	    (setq run nil
		  point-relative (- oldpoint (car last-sep))
		  ret (list (cons (cdr last-last-sep)
				  (car sep))
			    ;; reverse order
			    (buffer-substring (cdr last-sep)
					      (car sep))
			    (buffer-substring (car last-sep)
					      (cdr last-sep))
			    (buffer-substring (cdr last-last-sep)
					      (car last-sep))))
	    (when (< 1 (seq-count #'seq-empty-p (cdr ret)))
	      (setq ret nil)))
	   ((> (car last-sep) oldpoint) (setq run nil))
	   (t (setq last-last-sep last-sep
		    last-sep sep))))
	(if (not ret)
	    (goto-char oldpoint)
	  (goto-char (caar ret))
	  (kill-region (caar ret) (cdar ret))
	  (mapc #'insert (cdr ret))
	  (goto-char (+ (caar ret) (length (cadr ret)) point-relative)))))))

(defun cppedit--swap-regions (a-start a-end b-start b-end)
  (save-excursion
    (let* ((first (if (< a-start b-start)
		      (cons a-start a-end)
		    (cons b-start b-end)))
	   (second (if (= (car first) a-start)
		       (cons b-start b-end)
		     (cons a-start a-end)))
	   (f-str (buffer-substring (car first) (cdr first)))
	   (s-str (buffer-substring (car second) (cdr second)))
	   (length-diff (- (length s-str) (length f-str))))
      (when (> (cdr first) (car second))
	(error "Cannot swap overlapping regions"))
      (setq second (cons (+ length-diff (car second))
			 (+ length-diff (cdr second))))
      (goto-char (car first))
      (kill-region (car first) (cdr first))
      (insert s-str)
      (goto-char (car second))
      (kill-region (car second) (cdr second))
      (insert f-str))))
