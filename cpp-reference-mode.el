;; -*- lexical-binding: t -*-

;; TODO: Download the documentation if it doesn't exist and set
;;       cpp-reference-...-path variables accordingly
;; TODO: Create a mode for viewing the documentation

(require 'dom)
(require 'xref)
(require 'eww)

(defcustom cpp-reference-index-path nil
  "Path to a directory where cpp-reference's index xml files are stored."
  :type '(directory)
  :group 'cpp-reference)

(defcustom cpp-reference-wiki-path nil
  "Path to a directory where cpp-reference wiki's Main_Page.html is stored."
  :type '(directory)
  :group 'cpp-reference)

(defun cpp-reference--c-link-p (dom)
  (when-let ((link (dom-attr dom 'link)))
      (string-match "^c/" link)))

(defun cpp-reference--link-global-p (dom)
  (when-let ((link (dom-attr dom 'link)))
    (string-match "^c\\(pp\\)?/" link)))

(defun cpp-reference--name-global-p (dom)
  (or (cpp-reference--c-link-p dom)
      (when-let ((name (dom-attr dom 'name)))
	  (string-match "^std::" name))))

(defun cpp-reference--class-name (dom)
  (unless (equal (dom-tag dom) 'class)
    (error
     "[cpp-reference] cpp-reference--class-name: DOM is not tagged a class."))
  (let ((name (dom-attr dom 'name)))
    (save-match-data
      (string-match "::\\([^:]*\\)$" name)
      (match-string-no-properties 1 name))))

(defalias 'cpp-reference--constructor-name #'cpp-reference--class-name)
(defun cpp-reference--destructor-name (dom)
  (concat "~" (cpp-reference--class-name dom)))

(defun cpp-reference--get-link (dom parent)
  (let* ((dom-link (dom-attr dom 'link))
	 (link-global (cpp-reference--link-global-p dom))
	 (dom-tag (dom-tag dom)))
    ;; if there is a link - check if it's global or relative to parent
    ;; if there is none, check if there is an alias attribute, in that case
    ;; return nil so other utilities can resolve that
    ;; if there is no alias and no link, check if the item is a constructor
    ;; or a destructor, if not - name should be used as a link
    (if dom-link
	(if link-global
	    dom-link
	  (concat (cpp-reference--get-link parent nil)
		  (unless (string= dom-link ".")
		    (concat "/" dom-link))))
      (unless (dom-attr dom 'alias)
	(if parent
	    (concat (cpp-reference--get-link parent nil)
		    "/"
		    (pcase dom-tag
		    ('constructor (cpp-reference--constructor-name parent))
		    ('destructor (cpp-reference--destructor-name parent))
		    (_ (dom-attr dom 'name))))
	  (dom-attr dom 'name))))))

;; (defun cpp-reference--get-link (dom parent)
;;   (if (cpp-reference--link-global-p dom)
;;       (file-truename (concat (file-name-as-directory cpp-reference-wiki-path)
;; 			     (dom-attr dom 'link)))
;;     (and parent
;; 	 (file-truename (concat (file-name-as-directory
;; 				 cpp-reference-wiki-path)
;; 				(dom-attr parent 'link)
;; 				(let ((link (dom-attr dom 'link)))
;; 				  (if link
;; 				      (unless (string= link ".")
;; 					(concat "/" link))
;; 				    ;; (concat "/" (dom-attr dom 'name))
;; 				    nil)))))))

(defun cpp-reference--add-parens-to-fun (str)
  (if (string-match "operator " str)
      (concat str "()")
    (save-match-data
      (string-match (rx (or whitespace eol)) str)
      (string-trim-right
       (replace-match "() " nil nil str)))))

;; (defun cpp-reference--check-generic (dom name)
;;   (let ((dom-name (dom-attr dom 'name)))
;;     (when (string-match-p name dom-name)
;;       (list dom-name
;; 	    (symbol-string (dom-tag dom))
;; 	    (cpp-reference--get-link dom nil)))))
;; (defalias 'cpp-reference--check-const #'cpp-reference--check-generic)
;; (defalias 'cpp-reference--check-function #'cpp-reference--check-generic)
;; (defalias 'cpp-reference--check-variable #'cpp-reference--check-generic)

(defvar cpp-reference-database
  (make-hash-table :test #'equal :size 7000)
  "Database of all cpp-reference entries.

The key is a string with a name of the entry.
The value is a pair (TYPE . LINK). LINK is an absolute path to entry's doc.")

(defun cpp-reference--get-index-dom (index-filename)
  (with-temp-buffer
    (insert-file-contents-literally (concat (file-name-as-directory
					     cpp-reference-index-path)
					    index-filename))
    (libxml-parse-xml-region (point-min) (point-max) nil t)))

(defun cpp-reference--get-realpath (relative-path)
  (when relative-path
    (setq relative-path (concat (file-truename (file-name-as-directory
				       cpp-reference-wiki-path))
		       relative-path))
    (let* ((split (seq-partition relative-path
				 (string-match "[^/]*$" relative-path)))
	   (filename (cadr split))
	   (directory (car split)))
      (concat directory
	      ;; FIXME: this should be a regular error
	      (condition-case err
		  (file-name-completion (concat filename ".") directory)
		(file-missing
		 (with-current-buffer
		     (get-buffer-create "*cpp-reference-errors*")
		   (insert (format "Couldn't complete filename `%s' in directory `%s'.\n"
				   filename directory)))
		 nil))))))

(defun cpp-reference--add-to-database (key value)
  (cl-do ((key-base key)
	  (count 1 (1+ count)))
      ((not (gethash key cpp-reference-database))
       (puthash key value cpp-reference-database))
    (setq key (format "%s <%s>" key-base count))))

(defun cpp-reference--parse-cpp-search-app-file ()
  (let ((search-app-file (concat (file-name-as-directory
				  cpp-reference-index-path)
				 "index-cpp-search-app.txt")))
    (when (file-exists-p search-app-file)
      (with-temp-buffer
	(insert-file-contents-literally search-app-file)
	(save-match-data
	  (while (re-search-forward "^\\(.*?\\) => \\(.+\\)$" nil 'noerror)
	    (puthash (match-string-no-properties 1)
		     (cons "help" (match-string-no-properties 2))
		     cpp-reference-database)))))))

;; TODO: Maybe use search-{c,cpp} files generated by cppreference-doc.
;;       To generate them invoke 'make indexes'.
;; TODO: Handle 'inherits type child (and remove it from the ignore list).
;; TODO: type in a hashtable should be just plain 'function, 'type etc.
;; TODO: Break this function up into smaller pieces.
(defun cpp-reference--build-database ()
  (let ((aliases))
    (dolist (index '("index-functions-cpp.xml" "index-functions-c.xml"
		     "index-chapters-cpp.xml" "index-chapters-c.xml"))
      (dolist (node (dom-children (cpp-reference--get-index-dom index)))
	(let* ((item-name (dom-attr node 'name))
	       (item-type (dom-tag node)))
	  (when item-name
	    (let ((link (cpp-reference--get-link node nil)))
	      (cpp-reference--add-to-database
	       item-name
	       (cons (symbol-name item-type) link))
	      (unless link
		(when-let ((alias (dom-attr node 'alias)))
		  (setq aliases (cons (cons item-name alias)
				      aliases))))))
	  (dolist (child (dom-children node))
	    (when (and child (listp child))
	      (let* ((child-type (dom-tag child))
		     (child-name
		      (pcase child-type
			('constructor
			 (format "%s()"
				 (cpp-reference--constructor-name node)))
			('destructor
			 (format "%s()"
				 (cpp-reference--destructor-name node)))
			((or 'function 'overload)
			 (cpp-reference--add-parens-to-fun
			  (dom-attr child 'name)))
			('specialization
			 (format "%s (%s specialization)"
				 (dom-attr child 'name) item-name))
			((or 'comment 'inherits))
			(_ (dom-attr child 'name)))))
		(when child-name
		  (unless (string-match-p "^std::" child-name)
		    (setq child-name (concat item-name "::" child-name)))
		  (let ((link (cpp-reference--get-link child node)))
		    (cpp-reference--add-to-database
		     child-name
		     (cons (concat item-name " " (symbol-name child-type))
			   link))
		    (unless link
		      (when-let ((alias (dom-attr child 'alias)))
			(setq aliases (cons (cons child-name alias)
					    aliases))))))))))))
    (dolist (alias aliases)
      (setcdr (gethash (car alias) cpp-reference-database)
	      (cdr (gethash (cdr alias) cpp-reference-database))))
    ;; Add "index-cpp-search-app.txt" contents to the database
    (cpp-reference--parse-cpp-search-app-file)))

;; TODO: Save the timestamp of index files and check against it if files
;;       changed, rebuild the database if so.
(defun cpp-reference--initialize-database ()
  (when (null cpp-reference-index-path)
    (user-error "[cpp-reference] cpp-reference-index-path is not set"))
  (when (and cpp-reference-database
	     (= 0 (hash-table-count cpp-reference-database))
	     (file-exists-p cpp-reference-index-path))
    (message "[cpp-reference] Building the database...")
    (cpp-reference--build-database)
    (message "[cpp-reference] Database built successfully.")))

(defun cpp-reference--identifier-at-point ()
  (if-let ((identifier (symbol-at-point)))
      (symbol-name identifier)
    ""))

(defun* cpp-reference--dom-remove (dom &key id class)
  "Remove all matched descendants and of DOM.

:id and :class can be regexps or lists of regexps."
  (dolist (child (dom-non-text-children dom))
    (cond ((or (cpp-reference--dom-match-attr-p child 'id id)
	       (cpp-reference--dom-match-attr-p child 'class class))
	   (delq child dom))
	  (t
	   (cpp-reference--dom-remove child :id id :class class)))))

(defun cpp-reference--dom-match-attr-p (dom attr reg)
  "REG can be a regex or a list of regexes."
  (when-let ((attr-val (dom-attr dom attr)))
    (or (and (stringp reg)
	     (string-match-p reg attr-val))
	(and (consp reg)
	     (seq-some (lambda (r) (string-match-p r attr-val))
		       reg)))))

(defun cpp-reference--dom-detect-charset (dom)
  (catch 'return
    (dolist (meta (dom-by-tag dom 'meta))
      (when-let ((charset (dom-attr meta 'charset)))
	(throw 'return charset)))))

(defun cpp-reference--render-page (url buffer)
  (let* ((filename (car (url-path-and-query (url-generic-parse-url url))))
	 (dom (with-temp-buffer
		  (insert-file-contents filename)
		  (libxml-parse-html-region (point-min) (point-max)))))
    (cpp-reference--dom-remove dom :class '("t-navbar$" "noprint"
					    "editsection"))
    (with-current-buffer buffer
      (eww-setup-buffer)
      (plist-put eww-data :url url)
      (eww-display-html (or (cpp-reference--dom-detect-charset dom)
			    "utf-8")
			url
			(list 'base `((href . ,url))
			      dom)
			;; (point-min)
			nil
			(current-buffer))
      (eww-update-header-line-format))))

(define-minor-mode cpp-reference-mode
  "Mode for browsing C++ documentation."
  nil
  " cppref"
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "i") 'cpp-reference)
    map))

(defun cpp-reference-follow-link (&optional external mouse-event)
  (interactive (list current-prefix-arg last-nonmenu-event))
  ;; if it's not a local file in cpp-reference-wiki-path, run normal
  ;; eww-follow-link
  (if-let* ((url (get-text-property (point) 'shr-url))
	    (parsed (url-generic-parse-url url))
	    (path (car (url-path-and-query parsed)))
	    (filep (string= (url-type parsed) "file"))
	    (cpprefp (file-in-directory-p path cpp-reference-wiki-path)))
      (if (eww-same-page-p url (plist-get eww-data :url))
	  (when-let ((point (next-single-property-change
			     (point-min) 'shr-target-id)))
	    (goto-char point))
	(cpp-reference--render-page url (current-buffer)))
    (eww-follow-link external mouse-event)))

(defun cpp-reference--follow-link-advice (oldfun &rest args)
  "Use `cpp-reference-follow-link' if in `cpp-reference-mode'."
  (apply (if cpp-reference-mode
	     #'cpp-reference-follow-link
	   oldfun)
	 args))
(advice-add 'eww-follow-link :around #'cpp-reference--follow-link-advice)

;;;###autoload
(defun cpp-reference ()
  "Show the documentation of a thing at point."
  (interactive)
  (cond
   ((hash-table-empty-p cpp-reference-database)
    (cpp-reference--initialize-database))
   ((null cpp-reference-wiki-path)
    (user-error "[cpp-reference] cpp-reference-wiki-path is not set")))
  (let ((symbol (if ivy-mode
		    (ivy-read "Symbol: "
			    cpp-reference-database
			    :require-match t
			    :preselect (cpp-reference--identifier-at-point)
			    :history 'cpp-reference--read-history)
		  (completing-read "Symbol: "
				   cpp-reference-database
				   nil t
				   (cpp-reference--identifier-at-point)
				   'cpp-reference--read-history))))
    (with-current-buffer (get-buffer-create "*cpp-reference*")
      (pop-to-buffer (current-buffer))

      (cpp-reference--render-page
       (concat "file://" (cpp-reference--get-realpath
			  (cdr (gethash symbol cpp-reference-database))))
       (current-buffer))
      (unless cpp-reference-mode (cpp-reference-mode 1)))))

(provide 'cpp-reference)


;; Helper to list files unreachable from the index:
;; (let* ((default-directory "~/cppreference-doc/reference/en")
;;        (all-files (process-lines "find" "." "-type" "f" ))
;;        (index-files '()))
;;   (with-temp-buffer
;;     (dolist (idx
;; 	     '("autolink-c" "autolink-cpp" "highlight-c"
;; 	       "highlight-cpp" "search-c" "search-cpp")
;; 	     ;; '("search-c" "search-cpp")
;; 	     )
;;       (insert-file-contents (concat "~/cppreference-doc/output/indexes/"
;; 				    idx))
;;       (goto-char (point-min))
;;       (while (search-forward "=> " nil t)
;; 	(push (concat "./"
;; 		      (buffer-substring-no-properties (point) (point-at-eol))
;; 		      ".html")
;; 	      index-files))
;;       (setq index-files (seq-uniq index-files)))
;;     (with-temp-buffer
;;       (mapc (lambda (item)
;; 	      (insert item "\n"))
;; 	    (seq-difference all-files index-files))
;;       (write-file "/tmp/difference.txt"))))
