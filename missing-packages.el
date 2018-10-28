;;; missing-packages.el --- Check host packages -*- lexical-binding: t; -*-

(defvar pkg-config-present
  (with-temp-buffer
    (call-process "whereis" nil (current-buffer) nil "-b" "pkg-config")
    (goto-char 0)
    (when (search-forward ": " nil t) t))
  "t if pkg-config is present on the system, nil if not")

(defun my-find-package-on-host (name)
  "Check host system for package NAME.

If pkg-config is present on the system, the result is more precise:
Return nil if not found, 'exe if found an executable, 'lib if found a library.

If pkg-config is not present on the host system this function internally
uses whereis linux command:
Return nil if not found, t if found."
  (if pkg-config-present
      (cond
       ((eq 0 (call-process "which" nil nil nil name))
	'exe)
       ((eq 0 (call-process "pkg-config" nil nil nil "--exists" name))
	'lib))
    (with-temp-buffer
      (call-process "whereis" nil (current-buffer) nil "-b" name)
      (goto-char 0)
      (when (search-forward ": " nil t) t))))

(defun my-find-python-package (name)
  "Check host system for python package NAME.

Return path on success, nil of failure."
  (let ((result
	 (replace-regexp-in-string
	  "\n" ""
	  (shell-command-to-string
	   (concat
	    "find /usr/lib/$(basename $(readlink /usr/bin/python))*/site-packages -maxdepth 1 -name "
	    name)))))
    (unless (string-equal result "")
      result)))

(defun my-check-missing-packages-on-host (package-list &optional python)
  "Check host system for packages.

PACKAGE-LIST is a list of strings.
If PYTHON is not nil check also for python packages.

Return a list of missing packages or nil if didn't found any missing."
  (let ((result (list)))
    (dolist (item package-list)
      (unless
	  (or (my-find-package-on-host item)
	      (and python (my-find-python-package item)))
	(add-to-list 'result item)))
    result))

(defun my-print-missing-packages-as-warnings
    (warn-type package-list &optional python)
  "Check list of packages and display warning if found any missing.

WARN-TYPE can be a name of package that requires PACKAGE-LIST.
PACKAGE-LIST should be a list of package names as a list of strings.
If PYTHON is not nil check also for python packages.

Return t if any of the packages are missing, nil otherwise."
  (let ((missing-packages
	 (my-check-missing-packages-on-host package-list python)))
    (when missing-packages
      (display-warning
       (concat "Missing host packages - " warn-type)
       (mapconcat #'identity missing-packages ", ")
       :emergency))))
