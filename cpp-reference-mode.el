;; -*- lexical-binding: t -*-

;; TODO: Download the documentation if it doesn't exist and set
;;       cpp-reference-...-path variables accordingly
;; TODO: Create a mode for viewing the documentation

(require 'dom)
(require 'subr)
(require 'xref)

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
  (make-hash-table :test #'equal :size 5500)
  "Database of all cpp-reference entries.

The key is a string with a name of the entry.
The value is a pair (TYPE . LINK). LINK is an absolute path to entry's doc.")

(defun cpp-reference--get-index-dom (index-filename)
  (with-temp-buffer
    (insert-file-contents (concat (file-name-as-directory
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

;; TODO: Handle 'inherits type child (and remove it from the ignore list)
;; TODO: If there is already an entry for an item, add <number> to its name.
;;       Also uncomment chapter files.
;; TODO: type in a hashtable should be just plain 'function, 'type etc.
(defun cpp-reference--build-database ()
  (let ((aliases))
    (dolist (index '("index-functions-cpp.xml" "index-functions-c.xml"
		     ;; "index-chapters-cpp.xml" "index-chapters-c.xml"
		     ))
      (dolist (node (dom-children (cpp-reference--get-index-dom index)))
	(let* ((item-name (dom-attr node 'name))
	       (item-type (dom-tag node)))
	  (when item-name
	    (let ((link (cpp-reference--get-link node nil)))
	      (puthash item-name
		       (cons (symbol-name item-type)
			     link)
		       cpp-reference-database)
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
			((or 'comment 'inherits))
			(_ (dom-attr child 'name)))))
		(when child-name
		  (unless (string-match-p "^std::" child-name)
		    (setq child-name (concat item-name "::" child-name)))
		  (let ((link (cpp-reference--get-link child node)))
		    (puthash child-name
			     (cons (concat item-name " "
					   (symbol-name child-type))
				   link)
			     cpp-reference-database)
		    (unless link
		      (when-let ((alias (dom-attr child 'alias)))
			(setq aliases (cons (cons child-name alias)
					    aliases))))))))))))
    (dolist (alias aliases)
      (setcdr (gethash (car alias) cpp-reference-database)
	      (cdr (gethash (cdr alias) cpp-reference-database))))))

;; TODO: Save the timestamp of index files and check against it if files
;;       changed, rebuild the database if so.
(when (and cpp-reference-database
	   (= 0 (hash-table-count cpp-reference-database))
	   (file-exists-p cpp-reference-index-path))
  (message "[cpp-reference] Building the database...")
  (cpp-reference--build-database)
  (message "[cpp-reference] Database built successfully."))

;;;###autoload
(defun cpp-reference ()
  "Show the documentation of a thing at point."
  (interactive)
  (eww (concat "file://"
	       (cpp-reference--get-realpath
		(cdr (gethash
		      (completing-read "Symbol: "
				       cpp-reference-database
				       nil t
				       (xref-backend-identifier-at-point
					(xref-find-backend))
				       'cpp-reference--read-history)
		      cpp-reference-database))))))

(provide 'cpp-reference)

;; TODO: Remove the section underneath

;; (with-current-buffer (get-buffer-create "cpp-ref")
;;   (erase-buffer)
;;   (maphash (lambda (key value)
;; 	     (insert key " " (format "%s" value) "\n"))
;; 	   cpp-reference-database)
;;   (pop-to-buffer (current-buffer)))

;; (pp-eval-expression
;;  '(with-temp-buffer
;;     (insert-file-contents (concat cpp-reference-index-path
;; 				  "index-functions-cpp.xml"))
;;     (libxml-parse-xml-region (point-min) (point-max) nil t)))
