;;; missing-packages.el --- Check host packages -*- lexical-binding: t; -*-

;; (defun my-find-package-on-host (name)
;;   "Check host system for package NAME.

;; If pkg-config is present on the system the libraries can also be found.

;; Return nil if not found,
;;        'exe if found an executable,
;;        'lib if found a library."
;;   (cond
;;    ((executable-find name) 'exe)
;;    ((or (and (executable-find "which")
;; 	     (eq 0 (call-process "which" nil nil nil name)))
;; 	(and (executable-find "whereis")
;; 	 (with-temp-buffer
;; 	   (call-process "whereis" nil (current-buffer) nil "-b" name)
;; 	   (goto-char 0)
;; 	   (search-forward ": " nil t))))
;;     'exe)
;;    ((and (executable-find "pkg-config")
;; 	 (eq 0 (call-process "pkg-config" nil nil nil "--exists" name)))
;;     'lib)
;;    (t
;;     (with-temp-buffer
;;       (when (eq 0 (call-process shell-file-name
;; 				nil (current-buffer) nil
;; 				shell-command-switch (format "type %s" name)))
;; 	(goto-char 0)
;; 	(when (string-match-p (format "%s is /[^[:space:]]*" name)
;; 			      (buffer-substring-no-properties (point-at-bol)
;; 							      (point-at-eol)))
;; 	  'exe))))))

;; (defvar python-site-path
;;   (format "/usr/lib/%ssite-packages/"
;; 	  (file-name-completion
;; 	   (file-name-base (file-symlink-p "/usr/bin/python"))
;; 	   "/usr/lib/")))

(require 'subr-x)
(require 'seq)

(defvar python-site-path
  (split-string (shell-command-to-string
		     (combine-and-quote-strings '("python"
						  "-c"
						  "import sys
for p in sys.path:
    if 'site-packages' in p:
        print(p)"))))
  "Python's site module directories as a list.")


(defun my-find-python-package (name)
  (let ((candidate-files
	 (mapcan (lambda (path-entry)
		   (let ((filename (concat (file-name-as-directory path-entry)
					   name)))
		     (list filename (concat filename ".py"))))
		 python-site-path)))
    (seq-find #'file-exists-p candidate-files)))

(defun my-find-library (name)
  "Check host system for a library NAME.

Return t if found, nil otherwise.

This function require `pkg-config' to be present on the host system.
Otherwise it throws an error."
  (eq 0 (call-process "pkg-config" nil nil nil name)))

(defun my-missing-executables (executables)
  "Return a list of EXECUTABLES that are missing from the host system.

EXECUTABLES is a list of strings."
  (seq-remove #'executable-find executables))

(defun my-missing-python-packages (executables)
  "Return a list of python EXECUTABLES that are missing from the
host system.

EXECUTABLES is a list of strings."
  (seq-remove #'my-find-python-package executables))

(defun my-missing-libraries (libraries)
  "Return a list of LIBRARIES that are missing from the host system.

LIBRARIES is a list of strings.

This function requires `pkg-config' to be present on host.
Otherwise it throws an error."
  (seq-remove #'my-find-library libraries))

(defun my-missing-host-packages (packages &optional libs)
  "Return a list of PACKAGES that are missing from the host system.

PACKAGES is a list of strings. List can include executables, python packages
or libraries (if `pkg-config' is present on the host system, otherwise it will
return all libraries as missing).

By default it searches only for executables and python packages.
Searching for libraries can be switched on by setting LIBS to non-nil.
Note: Searching for libraries is very slow (more than 1000 times slower)."
  (let ((result (my-missing-python-packages
		 (my-missing-executables packages))))
    (if (and libs (executable-find "pkg-config"))
	(my-missing-libraries result)
      result)))

(defmacro with-check-for-missing-packages (packages warn-type libs
						    &rest body)
  "Check if all PACKAGES from the list are present on the host system.

Print a warning if any of them is not present. Otherwise eval the BODY.
If LIBS is not nil, check also for libraries (requires `pkg-config')."
  (declare (indent 3))
  (let ((missing (my-missing-host-packages packages libs)))
    (if missing
	`(display-warning ,warn-type
			  (format "Missing host packages: %s"
				  (quote ,missing)))
      `(progn ,@body))))
