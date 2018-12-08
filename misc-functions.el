;; -*- lexical-binding: t -*-
;; THIS FILE SHOULD NOT BE EVALUATED
;; It stores non-used functions as examples and reference

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


(defun open-file-temporary ()
  (with-temp-buffer
    (insert-file-contents fPath)
    (do-stuff)))

;; IN-PROGRESS

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

(defun my/c++-new-class (class-name)
  (interactive (let ((class-name (read-string "Class name: ")))
		 (list class-name)))
  (let ((project-root (my/c++-find-project-root)))
    ))


;; TODO: if process finished before setting a sentinel, just run BODY
(defmacro my-eval-after-process (proc &rest body)
  "Evaluate BODY after PROC finished.

Doesn't block the main process.

PROC must be a process."
  (let ((process (cond ((processp proc) proc)
		       ((consp proc) (eval proc)))))
    (unless (processp process)
      (signal 'wrong-type-argument '("PROC must be a process")))
    (set-process-sentinel
     process
     `(lambda (proc msg)
	;; pass to original sentinel
	(funcall #',(process-sentinel process) proc msg)
	(when (string-equal msg "finished\n")
	  ,@body))))
  nil)
;; (defun my-wait-for-process (process)
;;   "Wait for asynchronous process to finish."
;;   (assert (processp process))
;;   (set-process-sentinel
;;    process
;;    `(lambda (proc msg)
;;       ;; pass to original sentinel
;;       (funcall #',(process-sentinel process) proc msg)
;;       (when (string-equal msg "finished\n")
;; 	(message "fin"))))
;;   nil)


(let* ((xml (car '((api nil (query-continue nil (allpages ((apcontinue . c/string/multibyte/char16_t)))) (query nil (allpages nil (p ((pageid . 6047) (ns . 0) (title . c/string/byte/toupper))) (p ((pageid . 5758) (ns . 0) (title . c/string/multibyte))) (p ((pageid . 6111) (ns . 0) (title . c/string/multibyte/btowc))) (p ((pageid . 9392) (ns . 0) (title . c/string/multibyte/c16rtomb))) (p ((pageid . 9393) (ns . 0) (title . c/string/multibyte/c32rtomb)))))))))
       (continue (xml-get-attribute
		  (xml-query '(query-continue allpages) xml)
		  'apcontinue))
       (query (xml-query '(query) xml)))
  (pop-to-buffer (get-buffer-create "*xml-shenanigans*"))
  (erase-buffer)
  (dolist (page (xml-query-all '(query allpages p) xml))
    (insert (symbol-name (xml-get-attribute page 'title)) ?\n)))

(defun my-get-cppreference ()
  (let ((url-template "https://en.cppreference.com/mwiki/api.php?action=query&list=allpages&aplimit=500&apcontinue&format=xml&apfrom=%s")
       (apfrom "")
       (buf (get-buffer-create "*page-list*")))
    (loop
     for url = (format url-template apfrom)
     for xml = (car (with-current-buffer (url-retrieve-synchronously url)
		      (xml-parse-region)))
     do
     (setq apfrom (xml-get-attribute
		   (xml-query '(query-continue allpages) xml)
		   'apcontinue))
     (with-current-buffer buf
       (dolist (page (xml-query-all '(query allpages p) xml))
	 (insert (format "%s\n" (xml-get-attribute page 'title)))))
     until (string= "" apfrom))))
(my-get-cppreference)

(xml-get-attribute (xml-query '(query-continue allpages) (car (with-current-buffer (url-retrieve-synchronously "https://en.cppreference.com/mwiki/api.php?action=query&list=allpages&aplimit=500&apcontinue&format=xml&apfrom=old/wiki/string/c/strtok") (xml-parse-region))))
'apcontinue)


;; REGEX for variables:
;; "\\<\\(?1:[[:word:]-_<>: ]*\\)\
;; \\(?:\s+\\(?2:[*&]+\\)?\\|\\(?2:[*&]+\\)\s+\\)\
;; \\(?3:[[:word:]-_]+\\)\
;; \s?[=(]?"

;; TEST CASES:
;; unsigned int foo_;
;; std::vector<unsigned int> bar;
;; char* foo;
;; char *foo;
;; std::string& foo;
;; const std::string& foo;

;; int foo = 5;
;; std::string foo = "blabla";
;; std::string foo("blabla");
