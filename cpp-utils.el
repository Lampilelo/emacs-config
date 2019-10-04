;; -*- lexical-binding: t -*-

(require 's)
(require 'subr-x)

(defun my/c++--find-project-name ()
  (let* ((project-root (my/c++--find-project-root))
	 (search-props (cond ((file-exists-p
			       (concat project-root "meson.build"))
			      '("meson.build" . "'"))
			     ((file-exists-p
			       (concat project-root "CMakeLists.txt"))
			      '("CMakeLists.txt" . "\"")))))
    (unless (eq search-props nil)
      (with-temp-buffer
       (insert-file-contents (concat project-root (car search-props)))
       (search-forward (concat "project(" (cdr search-props)))
       (let ((start (point)))
    	 (search-forward (cdr search-props))
    	 (buffer-substring start (1- (point))))))))

;;;###autoload
(defun my/c++-create-include-guard ()
  (interactive)
  (when-let* ((project-name (my/c++--find-project-name))
	      (guard-macro
	       (format "%s_%s_%s_"
		       (upcase (s-word-initials project-name))
		       (upcase (replace-regexp-in-string
				"[-_[:space:]]" ""
				(file-name-base (buffer-file-name))))
		       (upcase (file-name-extension (buffer-file-name))))))
    (insert "#ifndef " guard-macro ?\n)
    (insert "#define " guard-macro "\n\n")
    (save-excursion
      (insert "\n\n#endif  // " guard-macro))))

(require 'ivy)
(setq my-man-asio-directory
      "/ssd-data/lampilelo/Programming/asio/include/boost/asio/doxygen/man")
;;;###autoload
(defun my-man-asio ()
  (interactive)
  (let ((Man-switches (format "-M '%s'" my-man-asio-directory)))
    (man (ivy-read
	  "Man: "
	  (let ((completion-regexp-list '("\\`[^\\.]")))
	    (mapcar
	     (lambda (filename)
	       (replace-regexp-in-string ".3\\'" "" filename))
	     (file-name-all-completions "" (concat (file-name-as-directory
						    my-man-asio-directory)
						   "man3"))))
	  :require-match t
	  :preselect (ivy-thing-at-point)
	  :sort t))))

;;;###autoload
(defun my/c++-make-definition-in-src-file ()
  "Create a function definition of the declaration at point."
  (interactive)
  (let ((filename (concat (file-name-sans-extension (buffer-file-name))
			  ".cpp"))
	(namespace (save-excursion
		     (save-match-data
		       (search-backward-regexp
			"namespace \\([^{ ]+\\)" nil t)
		       (match-string 1))))
	(class-name (save-excursion
		      (goto-char (point-at-eol))
		      (c-beginning-of-statement-1)
		      (backward-up-list)
		      (c-beginning-of-statement-1)
		      (save-match-data
			(when (looking-at
			       "\\(?:class\\|struct\\) \\([^{ ]+\\)")
			  (match-string 1))))))
    (goto-char (point-at-bol))
    (kill-ring-save (point) (or (search-forward ";" (point-at-eol) t)
				(point-at-eol)))
    (find-file-other-window filename)
    (goto-char (point-min))
    ;; TODO: handle no namespace option
    (when namespace
      (if (search-forward (concat "namespace " namespace) nil t)
	  (progn
	    (search-forward "{")
	    (backward-char)
	    (forward-sexp)
	    (goto-char (point-at-bol))
	    (open-line 1)
	    (newline)
	    (yank))
	;; when the file exists already but doesn't have the namespace go to
	(when (file-exists-p filename)
	  (goto-char (point-max))
	  (search-backward "}" nil t)
	  (forward-char)
	  (insert "\n\n"))
	(insert (concat "namespace " namespace " {\n"))
	(yank)
	(insert "\n}")
	(previous-line)
	(goto-char (point-at-eol)))
      ;; find the beggining of the function name
      (indent-region (point-at-bol) (point-at-eol))
      (search-backward ")")
      (forward-char)
      (backward-sexp)
	;; only operator functions can have whitespaces in the name

      (or (search-backward "operator" (point-at-bol) t)
	  (and (search-backward-regexp "[[:space:]]" (point-at-bol) t)
	       (forward-char)))
      (insert (or (concat class-name "::") ""))
      (goto-char (point-at-eol))
      (delete-horizontal-space)
      (when (looking-back ";")
	(delete-char -1))
      (insert " {\n\n}")
      (backward-char 2)

      (provide 'cpp-utils))))
