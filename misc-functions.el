;; -*- lexical-binding: t -*-
;; THIS FILE SHOULD NOT BE EVALUATED
;; It stores non-used functions as examples and reference

(defun M-x-other-window ()
      "Call M-x in the other window."
      (interactive)
      (save-selected-window
        (other-window 1)
        (execute-extended-command nil)))

(defun nmapcar (proc lst)
  "Like mapcar but destructive (modifies the list in-place)."
  (let ((tail lst))
    (while tail
      (setcar tail (funcall proc (car tail)))
      (setq tail (cdr tail))))
  lst)

(defun my-pairs (lst)
  "Split list LST into pairs."
  (cl-check-type lst list)
  (let ((result))
    (while lst
      (when (cadr lst)
	  (setq result (cons (cons (car lst) (cadr lst)) result)))
      (setq lst (cddr lst)))
    result))


(require 'cl-lib)
(require 'subr-x)
(cl-defun my-id3tag (filename &key artist album song number)
  "Set id3 tags of an mp3 file."
  (unless (file-name-absolute-p filename)
    (user-error "[id3tag] File name must be absolute"))
  (unless (string= (file-name-extension filename) "mp3")
    (user-error "[id3tag] Not an mp3 file"))
  (setq filename (file-truename filename))
  (let ((id3-exec
	 (or (executable-find "id3v2")
	     (executable-find "id3tag")
	     (user-error "[id3tag] Could not find id3tag and id3v2")))
	(id3iconv-exec
	 (or (executable-find "mid3iconv")
	     (user-error "[id3tag] Could not find any id3iconv utility")))
	(args))
    (when artist (setq args (nconc (list "--artist" artist) args)))
    (when album (setq args (nconc (list "--album" album) args)))
    (when song (setq args (nconc (list "--song" song) args)))
    (when number (setq args (nconc (list "--track" number) args)))
    (setq args (nconc args (list filename)))
    (with-temp-buffer
      (unless (= 0 (apply #'call-process id3-exec nil t nil args))
	(user-error (format "[id3tag] %s" (string-trim (buffer-string)))))
      (erase-buffer)
      (unless (= 0 (call-process id3iconv-exec nil t nil filename))
      	(user-error (format "[id3tag] %s" (string-trim (buffer-string))))))))

;; IN-PROGRESS

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
;; This is an elisp version of dmenu's fuzzy sort
;; TODO: put it somehow into flx; OR make it self-sufficient and fast with
;;       caches and pre-processing
(defun my-sort (text cands)
  "Fuzzy sort.

The algorithm is dmenu's fuzzy sort."
  ;; iterate over candidates
  (condition-case nil
      (mapcar
       'car ; return only the resulting list without the sorting info
       (sort
	(cl-do* ((matches (list))
		 (text-len (length text))
		 (candidates cands (cdr candidates))
		 (candidate (car candidates) (car candidates))
		 (candidate-len (length candidate) (length candidate))
		 (start-idx -1 -1)
		 (end-idx -1 -1))
	    ((null candidates) matches)
	  (if (> text-len 0)
	      (progn
		;; iterate over chars in candidate
		(cl-do* ((cand-idx 0 (1+ cand-idx))
			 (text-idx 0)
			 (c nil))
		    ((or (= cand-idx candidate-len)
			 (= text-idx text-len)))
		  (setq c (elt candidate cand-idx))
		  (when (char-equal c (elt text text-idx))
		    (when (= start-idx -1)
		      (setq start-idx cand-idx))
		    (setq text-idx (1+ text-idx))
		    (when (= text-idx text-len)
		      (setq end-idx cand-idx))))
		;; build list of matches
		(unless (= end-idx -1)
		  ;; compute distance
		  ;; add penalty if match starts late
		  ;; add penalty for a long match without many matching
		  ;;     characters
		  (push (cons candidate
			      (+ (log (+ start-idx 2))
				 (- end-idx start-idx text-len)))
			matches)))
	    (push (cons candidate 0) matches)))
	;; sorting lambda
	(lambda (match1 match2)
	  (<= (cdr match1) (cdr match2)))))
    (error cands)))
