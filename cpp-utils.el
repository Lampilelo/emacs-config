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
