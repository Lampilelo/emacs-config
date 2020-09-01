;;; -*- lexical-binding: t; -*-

;;; Usage:

;; Existing search engines can be redefined by calling `define-search-engine'
;; using the same engine name.

(require 'dom)

(defvar web-search-engines nil
  "Search engines list.

Check `define-search-engine' for the documentation about the stored values.")

(defvar web-search-browser-function nil
  "Function to perform the search in a WWW browser.

This is used only when the search engine has no PARSE-FUNCTION.
More details in `define-search-engine' docstring.")

(defstruct search-engine name keyword template-uri parse-function)

(defun web-search--lisp-case (s)
  (downcase (replace-regexp-in-string "[ \t\n\r]+" "-" s)))

(defun web-search--maybe-pop-to-buffer (buffer)
  (cond ((bufferp buffer) (pop-to-buffer buffer))
	((stringp buffer) (message buffer))
	(t (message "Search failed."))))

(defun web-search--dom-remove-recursively (node-type dom)
  (dolist (child (dom-children dom))
    (when (listp child)
      (if (eq (dom-tag child) node-type)
	  (delq child dom)
	(web-search--dom-remove-recursively node-type child)))))

(defun web-search--dom-remove-by-attr-recursively (attr-type attr dom)
  (dolist (child (dom-children dom))
    (when (listp child)
      (let ((current-attr (dom-attr child attr-type)))
	(if (and current-attr (string-match-p attr current-attr))
	    (delq child dom)
	  (web-search--dom-remove-by-attr-recursively
	   attr-type attr child))))))

;; TODO: Maybe PARSE-FUNCTION shouldn't take the DOM object as an argument.
;; NOTE  Instead it should take url argument so it would be very simple to
;;       define search engines with PARSE-FUNCTIONs like eww-browse-url
;;       or web-search-browser-function.
;;       In that case how does it differ from setting
;;       `browse-url-default-browser' as a list of (REGEXP . FUNCTION) pairs?
;;       Maybe this file doesn't make sense?
;;;###autoload
(defun define-search-engine (name keyword template-uri parse-function)
  "Define a new search engine.

NAME is the legible name of the search engine.

KEYWORD is the keyword used to quickly select the engine.

TEMPLATE-URI is an URI for the search with \"%s\" in place of a search query.

PARSE-FUNCTION is a function that takes a singular argument that is a DOM
object of an HTML response from retrieving TEMPLATE-URI with the search query.
The return value can be:
- a fully rendered buffer,
- nil if the search was unsuccessfull - in that case the generic message would
  be shown to the user,
- a string with the custom message to be shown in the minibuffer.
The value of PARSE-FUNCTION can also be `nil'. In that case the search will be
performed with `browse-url-browser-function' or `web-search-browser-function' if it's not nil (this will take priority)."
  (cond ((not (stringp name))
	 (error "NAME should be a string"))
	((not (stringp keyword))
	 (error "KEYWORD should be a string"))
	((not (stringp template-uri))
	 (error "TEMPLATE-URI should be a string"))
	;; if parse-function is nil, do not check for it's correctness
	((null parse-function))
	((not (functionp parse-function))
	 (error "PARSE-FUNCTION should be a function"))
	((not (= (car (func-arity parse-function)) 1))
	 (error "PARSE-FUNCTION should take only one argument")))
  (dolist (search-engine web-search-engines)
    (cond
     ;; Remove search engine if the name is already on the list
     ;; FIXME: Corner case: if there is a search engine with the same name
     ;;        it will be deleted and then if the keyword is the same as
     ;;        in the another search engine it will throw an error leaving
     ;;        the list with removed engine but without the replacement
     ;;        I guess it's not the end of the world because the user would
     ;;        like to redefine the engine anyway.
     ((string= (search-engine-name search-engine) name)
      (setq web-search-engines (delq search-engine web-search-engines)))
     ((string= (search-engine-keyword search-engine) keyword)
      (error (format "Keyword `%s' already taken"
  			  keyword)))))
  (push (make-search-engine
       	 :name name
       	 :keyword keyword
       	 :template-uri template-uri
       	 :parse-function parse-function)
       	web-search-engines))

;;;###autoload
(defun web-search (search-string)
  "Search web with a search engine defined in `web-search-engines'.

SEARCH-STRING should be \"KEYWORD QUERY\" where KEYWORD is search engine's
keyword and QUERY is stuff you want to find."
  (interactive "sSearch string: "
	       (list search-string))
  (do* ((first-whitespace (string-match "[ \t\n\r]+" search-string))
	(se-keyword (substring-no-properties search-string
					     0 first-whitespace))
	(se-query (string-trim (substring-no-properties search-string
							first-whitespace)))
	(search-engines web-search-engines (cdr search-engines))
	(search-engine (car search-engines) (car search-engines)))
      ((or (null search-engines)
	   (string= (search-engine-keyword search-engine) se-keyword))
       (if search-engine
	   (if (search-engine-parse-function search-engine)
	       (web-search--maybe-pop-to-buffer
		(funcall `,(search-engine-parse-function search-engine)
	 		 (with-current-buffer
	 		     (url-retrieve-synchronously
	 		      (format
			       (search-engine-template-uri search-engine)
			       se-query))
	 		   (libxml-parse-html-region (point) (point-max)))))
	     (funcall (or web-search-browser-function
			  browse-url-browser-function)
		      (format (search-engine-template-uri search-engine)
			      se-query)))
	 (message "Keyword doesn't match any search engine.")))))


;; This is left for the reference.
;; Maybe in the future I'd like to create custom parser for resulting DOM
;; instead of shr.
;; (defun web-search--sjp (query)
;;   (let ((results
;; 	 (with-current-buffer (url-retrieve-synchronously
;; 			       (format web-search-sjp-uri-template query))
;; 	   ;; type-187125: Wielki słownik ortograficzny PWN
;; 	   ;; type-187126: Słownik Języka Polskiego PWN
;; 	   ;; type-187123: Słownik języka polskiego pod red. W. Doroszewskiego
;; 	   ;; type-187386: Porady językowe
;; 	   ;; type-187142: Encyklopedia PWN
;; 	   (dom-by-class (libxml-parse-html-region (point-min)
;; 						   (point-max))
;; 			 "type-187126"))))

;;     (with-current-buffer
;; 	(get-buffer-create (format "*web-search-sjp: %s*" query))
;;       (dolist ((entry results))
;; 	(insert (dom-text (dom-by-tag (dom-by-class entry "tytul")
;; 				      'a)))
;; 	(insert (string-trim (replace-regexp-in-string
;; 			      "•\\| " (dom-text entry))))))))

;; TODO: Move those to init.el or create a file for web-search configuration

(setq web-search-browser-function 'eww-browse-url)

(define-search-engine
  "Słownik Języka Polskiego PWN"
  "s"
  "https://sjp.pwn.pl/szukaj/%s"
  (lambda (dom)
    (setq dom (dom-by-class dom "search-content"))
    (if (dom-by-class dom "wyniki")
	(with-current-buffer
	    (get-buffer-create "*web-search-sjp*")
	  (read-only-mode -1)
	  (erase-buffer)
	  (web-search--dom-remove-recursively 'img dom)
	  (shr-insert-document dom)
	  (goto-char (point-min))
	  (view-mode 1)
	  (local-set-key (kbd "q") #'kill-this-buffer)
	  (current-buffer))
      nil)))

(define-search-engine
  "Oxford Dictionary"
  "o"
  "https://www.lexico.com/en/definition/%s"
  (lambda (dom)
    (setq dom (dom-by-class dom "entryWrapper"))
    (with-current-buffer
	(get-buffer-create "*web-search-oxford*")
      (read-only-mode -1)
      (erase-buffer)
      (shr-insert-document dom)
      (goto-char (point-min))
      (view-mode 1)
      (local-set-key (kbd "q") #'kill-this-buffer)
      (current-buffer))))

(defun web-search-glosbe (dom)
    (setq dom (dom-by-class dom "wordDetails"))
    (if (dom-by-class dom "alert")
	nil
      (with-current-buffer
	  (get-buffer-create "*web-search-glosbe*")
	(read-only-mode -1)
	(erase-buffer)
	(web-search--dom-remove-recursively 'img dom)
	;; (web-search--dom-remove-by-attr-recursively 'class "dropdown-menu" dom)
	(shr-insert-document dom)
	(goto-char (point-min))
	(view-mode 1)
	(local-set-key (kbd "q") #'kill-this-buffer)
	(current-buffer))))

(define-search-engine
  "Glosbe Translate Pl-En"
  "tpe"
  "https://glosbe.com/pl/en/%s"
  #'web-search-glosbe)

(define-search-engine
  "Glosbe Translate En-Pl"
  "tep"
  "https://glosbe.com/en/pl/%s"
  #'web-search-glosbe)

(define-search-engine
  "Glosbe Translate Pl-Fr"
  "tpf"
  "https://glosbe.com/pl/fr/%s"
  #'web-search-glosbe)

(define-search-engine
  "Glosbe Translate Fr-Pl"
  "tfp"
  "https://glosbe.com/fr/pl/%s"
  #'web-search-glosbe)

(define-search-engine
  "Wikipedia PL"
  "w"
  "https://pl.wikipedia.org/wiki/Special:Search?search=%s"
  nil)

;; TODO: This could be left in this file as a default and the reference for
;;       how to create an engine.
(define-search-engine
  "Wikipedia EN"
  "we"
  "https://en.wikipedia.org/wiki/Special:Search?search=%s"
  nil)
