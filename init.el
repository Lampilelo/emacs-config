;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs

(require 'package)
(package-initialize)
(setq package-archives
      '(
	;; ("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("gnu" . "https://elpa.gnu.org/packages/")
	("org" . "https://orgmode.org/elpa/")))

;; Privacy and security while downloading packages
;; it needs gnutls(-bin) and python-certifi packages to work
(require 'tls)
(let ((trustfile
       (replace-regexp-in-string
        "\\\\" "/"
        (replace-regexp-in-string
         "\n" ""
         (shell-command-to-string "python -m certifi")))))
  (setq tls-program
        (list
         (format "gnutls-cli%s --x509cafile %s -p %%p %%h"
                 (if (eq window-system 'w32) ".exe" "") trustfile)))
  (setq gnutls-verify-error t)
  (setq gnutls-trustfiles (list trustfile)))

;; USE-PACKAGE
(if (not (package-installed-p 'use-package))
    (progn
      (package-refresh-contents)
      (package-install 'use-package)))
(require 'use-package)
(setq use-package-always-ensure t)

(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
;; (global-hl-line-mode) ;; slows down next-line nad previous-line
(setq inhibit-startup-screen t)
(setq scroll-conservatively 100)
(setq sentence-end-double-space nil)

;; ido mode
;; TODO: check out https://masteringemacs.org/article/introduction-to-ido-mode
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
;; (ido-mode 1)

;; org mode customizations
(use-package org
  :ensure org-plus-contrib
  :pin org)
(require 'org)
(require 'org-agenda)
(require 'ox-latex)
(require 'ox-beamer)
(setq org-edit-src-content-indentation 2)
(setq org-src-fontify-natively t)
(setq org-src-tab-acts-natively t)
(setq org-startup-indented t)
(setq org-use-extra-keys t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "yellow1")
	("WAITING" . "gold2")))
;; Enable changing width of an image in org with #+ATTR_ORG: :width <width>
(setq org-image-actual-width nil)
(add-hook 'org-mode-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'visual-line-mode)
;; (setq org-ellipsis " ↴")

;; org-mode source coloring
;; Note 1: python-pygments needs to be installed
(setq org-latex-listings 'minted)
(setq org-export-with-smart-quotes t)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; Use pdf-tools instead of doc-view
(use-package pdf-tools
  :config
  (pdf-tools-install))
;; to uninstall you have to call (pdf-tools-uninstall)

(load "~/.emacs.d/org-agenda-init.el")

;; Polish quotation marks
(push
 '("pl"
   (opening-double-quote :utf-8 "„"  :html "&bdquo;"
			 :latex ",," :texinfo "@quotedblbase{}")
   (closing-double-quote :utf-8 "”"  :html "&rdquo;"
			 :latex "''" :texinfo "@quotedblright{}")
   (opening-single-quote :utf-8 "‚"  :html "&sbquo;"
			 :latex "," :texinfo "@quotesinglbase{}")
   (closing-single-quote :utf-8 "’"  :html "&rsquo;"
			 :latex "'" :texinfo "@quoteright{}")
   (apostrophe :utf-8 "’" :html "&rsquo;"))
 org-export-smart-quotes-alist)
;; Default to polish language for export
;; To change language per document add i.e. '#+LANGUAGE: en' to the org file
(setq org-export-default-language "pl")

(use-package org-bullets
  :config
  (add-hook 'org-mode-hook 'org-bullets-mode))


;; TODO: Check if we can convert current mode name to helm-info function
;; Relevant symbols:
;;   helm-default-info-index-list, helm-info-search-index
(defun my-contextual-helm-info (&optional generic-info)
  "If there is known defun for helm-info-<MODE> for current major mode, call it.
Otherwise call helm-info.
If GENERIC-INFO is non-nil, call generic helm-info.

With a prefix argument \\[universal-argument], just call generic helm-info."
  (interactive "P")
  (catch 'placeholder ;because normal return sucks, TODO: refactor this!
    (when generic-info			;if universal prefix argument is used
      (funcall 'helm-info)		;call helm-info and exit
      (throw 'placeholder "Defun called with a prefix argument"))
    (let ((defun-to-call
	    (intern 		;call defun by name
	     (let ((current-mode 	;get mode name that matches helm-info
		    (downcase (replace-regexp-in-string
			       "-mode" "" (symbol-name major-mode)))))

	       ;; Get defun name, e.g. helm-info-cpp
	       ;; Some modes are called differently in helm, so we need
	       ;; to rename them before evaluating
	       (concat "helm-info-"	;
		       (cond ((equal current-mode "c++") "cpp")
			     ((equal current-mode "emacs-lisp") "elisp")
			     ((equal current-mode "lisp-interaction") "elisp")
			     (t current-mode)))))))

      ;; check if helm-info-CURRENT_MODE exists, if so - call it
      ;; otherwise call generic helm-info
      (if (not (eq (symbol-function defun-to-call) nil))
	  (funcall defun-to-call)
	(funcall 'helm-info)))))
(global-set-key (kbd "C-h h") 'my-contextual-helm-info)

;; bind M-RET to open files externally with helm
(eval-after-load "helm-files"
  '(progn
     (define-key helm-find-files-map (kbd "M-RET")
       'helm-ff-run-open-file-with-default-tool)
     (define-key helm-generic-files-map (kbd "M-RET")
       'helm-ff-run-open-file-with-default-tool)))

;; bind M-RET to open files externally with dired
(defun dired-open-file-with-default-tool ()
  "Open FILE with the default tool on this platform."
  (interactive)
  (dired-do-shell-command
   (cond ((eq system-type 'gnu/linux)
	  "xdg-open")
	 ((or (eq system-type 'darwin) ;; Mac OS X
	      (eq system-type 'macos)) ;; Mac OS 9
	  "open"))
   nil (dired-get-marked-files)))
(define-key dired-mode-map (kbd "M-RET") 'dired-open-file-with-default-tool)

(column-number-mode 1)
(setq split-width-threshold 140)
(setq split-window-preferred-function (quote split-window-sensibly))

;; autosaves
;; create the autosave dir if necessary, since emacs won't.
(defvar my-autosave-directory "~/.emacs.d/autosaves/")
(make-directory my-autosave-directory t)
;; Put autosave files (ie #foo#) and backup files (ie foo~) in ~/.emacs.d/.
(setq backup-directory-alist
      `((".*" . ,my-autosave-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,my-autosave-directory t)))
(setq auto-save-list-file-prefix
      my-autosave-directory)

;; Set default browser for opening links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "vivaldi")

;; C++ default options
(setq c-default-style "linux"
      c-basic-offset 4)

;; GDB
(setq gdb-many-windows t
      ;; Display source file containing the main routine at startup
      gdb-show-main t)

;; Custom global keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)
(global-set-key (kbd "C-x k") 'kill-this-buffer)

;; Monday as first day of the week
(setq calendar-week-start-day 1)

;; ==================== FUNCTIONS ===================

;; Got it from here: http://www.draketo.de/light/english/emacs/babcore
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the FRAME to ARG:

- If arg is nil, unset the urgency.
- If arg is any other value, set the urgency.

If you unset the urgency, you still have to visit the frame to make the urgency setting disappear (at least in KDE)."
  (let* ((wm-hints (append (x-window-property
			    "WM_HINTS" frame "WM_HINTS" source nil t) nil))
	 (flags (car wm-hints)))
    (setcar wm-hints
	    (if arg
		(logior flags #x100)
	      (logand flags (lognot #x100))))
    (x-change-window-property "WM_HINTS" wm-hints frame "WM_HINTS" 32 t)))

(defun x-urgent (&optional arg)
  "Mark the current Emacs frame as requiring urgent attention.

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (selected-frame))
    (x-urgency-hint frame (not arg))))

(defun my-find-package-on-host (name)
  "Check host system for package NAME.
Returns path on success, nil on failure."
  (let ((cmd-output
	 (substring
	  (shell-command-to-string (concat "which " name))
	  0 -1)))	;substring because shell-command returns \n sign at the end
    ;; Return nil if not found and command output if found
    (when (not (string-equal cmd-output (concat name " not found")))
      cmd-output)))

(defun my-find-python-package (name)
  "Check host system for python package NAME.
Returns path on success, nil of failure."
  (let ((result
	 (replace-regexp-in-string
	  "\n" ""
	  (shell-command-to-string
	   (concat
	    "find /usr/lib/$(basename $(readlink /usr/bin/python))*/site-packages -maxdepth 1 -name "
	    name)))))
    (when (not (string-equal result ""))
      result)))

(defun my-check-missing-packages-on-host (package-list &optional python)
  "Check host system for packages.
PACKAGE-LIST is a list of strings. If PYTHON is not nil it checks also for
python packages.
Returns list of missing packages or nil if didn't found any missing."
  (let ((result (list)))
    (dolist (item package-list)
      (when (not
	     (if (not python)
		 (my-find-package-on-host item)
	       ;; if python
	       (or (my-find-package-on-host item)
		   (my-find-python-package item)))) ;if package wasn't found
	(add-to-list 'result item)))
    result))

(defun my-print-missing-packages-as-warnings
    (warn-type package-list &optional python) ;args
  "Check list of packages and display warning if found any missing.
WARN-TYPE can be a name of package that requres PACKAGE-LIST.
If PYTHON is not nil it checks also for python packages."
  (let ((missing-packages
	 (my-check-missing-packages-on-host package-list python)))
    (when missing-packages
      (display-warning
       (concat "Missing host packages - " warn-type)
       (mapconcat 'identity missing-packages ", ")
       :emergency))))

(defmacro measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let ((time (current-time)))
     ,@body
     (message "%.06f" (float-time (time-since time)))))

;; MOTD
(defun my-create-motd-buffer ()
  "Create *MOTD* buffer and switch to it."
  (switch-to-buffer (generate-new-buffer "*MOTD*")))

(defun sudo-find-file (file-name)
  "Like find file, but open the file as root.
FILE-NAME is path to the file."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(defun my-term-command (command &optional term-name pop-buffer)
  "Run COMMAND in term-mode in the default shell.

TERM-NAME will be the buffer name, if nil it defaults to *term*.

If POP-BUFFER not nil it will pop the buffer in a new window, otherwise in current."
  (interactive (let ((command (read-string "Command: ")))
		 (list command)))
  (let ((term-name (or term-name "term"))) ;default value for TERM-NAME
    (set-buffer (apply 'make-term term-name
  		       (getenv "SHELL")
  		       nil
  		       (list "-c" command)))
    (term-mode)
    (term-char-mode)
    (if pop-buffer
	(pop-to-buffer (concat "*" term-name "*"))
      (switch-to-buffer (concat "*" term-name "*")))))

  ;; ==================== PACKAGES ====================

  ;; TODO: customize company theming for tangotango and remove monokai
  ;; or customize coloring in monokai
  ;; (use-package monokai-theme
  ;;   :config
  ;;   (load-theme 'monokai t)
  ;;   (set-face-attribute 'default nil :height 120))

  ;; (use-package tangotango-theme
  ;;   :config
  ;;   (load-theme 'tangotango t))

(load-theme 'leuven t)
(set-face-attribute 'default nil :height 120)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#FFFFF7")))))

(use-package auto-complete)

;; IVY
(use-package flx)  ;better matching for Ivy
(use-package smex) ;better M-x, remembers frequently used commands
(use-package ivy
  :init
  ;; use fuzzy regex for everything but swiper
  (setq ivy-re-builders-alist
	'((t . ivy--regex-fuzzy)
	  (swiper . ivy--regex-plus)
	  (counsel-git . ivy--regex-plus)
	  (counsel-git-grep . ivy--regex-plus)))
  :config
  (ivy-mode t)
  :bind (:map ivy-minibuffer-map
	      ("C-s" . 'ivy-toggle-fuzzy)))

(use-package swiper
  :bind
  (("C-s" . swiper)
   ("M-s M-s" . isearch-forward)))
(use-package counsel
  :bind
  (("M-x" . counsel-M-x)
   ;; ("C-x C-f" . counsel-find-file) ;; switched to helm
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> l" . counsel-find-library)
   ("<f1> s" . counsel-info-lookup-symbol)
   ("<f1> S" . describe-syntax) ;; switched from "<f1> s"
   ("<f1> u" . counsel-unicode-char)
   ;; Attention: C-c bindings for git (may interfere with other modes)
   ("C-c g" . counsel-git)
   ("C-c j" . counsel-git-grep)
   ("C-c k" . counsel-ag)))

(use-package helm
  :config
  (require 'helm-config)
  ;; It's necessary to load helm-buffers so that helm-buffer-map is loaded
  (require 'helm-buffers)
  :bind
  (("C-x f" . helm-for-files)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-x C-b" . helm-buffers-list)
   :map helm-buffer-map
   ("C-k" . helm-buffer-run-kill-persistent)
   ("C-M-k" . helm-buffer-run-kill-buffers)))

(use-package flycheck
  :config
  (add-hook 'c-mode-common-hook 'flycheck-mode)
  (add-hook 'python-mode-hook 'flycheck-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :bind
  ("C-\"" . company-complete))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  :config
  :bind
  ;; In company-search-mode company-active-map is used
  ;; We need to exit that mode to call company-yasnippet
  ;; Also we pass all needed args to it
  ("C-'" . (lambda (command &optional arg &rest ignore)
	     (interactive (list 'interactive))
	     (company-abort)
	     (company-yasnippet command arg ignore))))
(use-package yasnippet-snippets)

;; ========================= DEPRECATED =========================
;;                     (left for reference)

;; ;;(shell-command )
;; (use-package rtags
;;   :init
;;   (setq rtags-path "~/.emacs.d/rtags/build/bin/")
;;   ;; helm as backend for displaying
;;   (setq rtags-display-result-backend 'helm)
;;   :config
;;   (rtags-enable-standard-keybindings)
;;   (my-print-missing-packages-as-warnings ;check for requirements on host
;;    "RTAGS"
;;    '("make" "cmake" "gcc" "clang" "git" "doxygen"))
;;   ;; Could use rtags package from aur, but it won't work on other systems
;;   ;; Maybe search for latest release with python-requests in rtags repo?
;;   (if (not (file-exists-p rtags-path))
;;       (shell-command "sh ~/.emacs.d/get-rtags.sh"))
;;   (rtags-start-process-unless-running)
;;   ;; Rebind keys for finding symbols and references
;;   :bind (:map c-mode-map
;; 	      ("C-." . rtags-find-symbol-at-point)
;; 	      ("C-," . rtags-find-references-at-point)
;; 	 :map c++-mode-map
;; 	      ("C-." . rtags-find-symbol-at-point)
;; 	      ("C-," . rtags-find-references-at-point)))

;; (use-package company-rtags
;;   :init
;;   (setq rtags-completions-enabled t)
;;   (setq rtags-autostart-diagnostics t)
;;   :config
;;   (push 'company-rtags company-backends))

;; (use-package flycheck-rtags
;;   :config
;;   (add-hook 'c-mode-common-hook
;; 	    (lambda ()
;; 	      "Flycheck RTags setup"
;; 	      (flycheck-select-checker 'rtags)
;; 		(message "flycheck-rtags lambda")
;; 	       ;;RTags creates more accurate overlays
;; 	       (setq-local flycheck-highlighting-mode nil)
;; 	       (setq-local flycheck-check-syntax-automatically nil))))

;; (use-package helm-rtags)

;; ;; TODO: build server if there's a new version
;; (use-package irony
;;   :config
;;   (if (not (file-exists-p "~/.emacs.d/irony/bin/irony-server"))
;;       (irony-install-server
;;        (format
;; 	(concat "%s %s %s && %s --build . "
;; 		"--use-stderr --config Release --target install")
;; 	(shell-quote-argument irony-cmake-executable)
;; 	(shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
;; 				      (expand-file-name
;; 				       irony-server-install-prefix)))
;; 	(shell-quote-argument
;; 	 (or irony-server-source-dir
;; 	     (expand-file-name "server"
;; 			       (file-name-directory
;; 				(find-library-name "irony")))))
;; 	(shell-quote-argument irony-cmake-executable))))  
;;   (add-hook 'c-mode-common-hook 'irony-mode)
;;   (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;   :bind (:map irony-mode-map
;; 	      ([remap completion-at-point] .
;; 	       irony-completion-at-point-async)
;; 	      ([remap complete-symbol] .
;; 	       irony-completion-at-point-async)))

;; (use-package company-irony
;;   :init
;;   (setq company-idle-delay 0)
;;   :config
;;   (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;;   (add-to-list 'company-backends 'company-irony)
;;   :bind (:map c-mode-map ("C-\"" . company-complete)
;; 	 :map c++-mode-map ("C-\"" . company-complete)))

;; (use-package flycheck-irony
;;   :config
;;   (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

;; ;; (use-package company-irony-c-headers
;;   ;; :config
;;   ;; (eval-after-load 'company
;;   ;;   '(add-to-list 'company-backends
;; 	       ;; '(company-irony-c-headers company-irony))))

;; (use-package cmake-mode)

;; (use-package cmake-ide
;;   :init
;;   (my-print-missing-packages-as-warnings "CMAKE-IDE" '("cmake"))
;;   (setq cmake-ide-rdm-executable "~/.emacs.d/rtags/build/bin/rdm")
;;   (setq cmake-ide-rdm-rc-path "~/.emacs.d/rtags/build/bin/")
;;   :bind
;;   (:map c++-mode-map
;; 	("C-c C-c" . 'cmake-ide-compile)))

;; ;; Eldoc to show function interface in minibuffer
;; (defun my-eldoc-hook ()
;;   (setq-local eldoc-documentation-function #'rtags-eldoc))
;; (add-hook 'c-mode-common-hook 'my-eldoc-hook)

;; ============================================================

;; Load cc-mode so that c++-mode-map is not void.
(require 'cc-mode)

(use-package lsp-mode
  :config
  ;; (require 'lsp-flycheck)
  ;; (require 'lsp-mode)
  (require 'lsp-mode)
  :bind
  (:map c++-mode-map
	("C-c r" . 'lsp-rename)))

(use-package lsp-ui
  :config
  (require 'lsp-ui)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

(use-package cquery
  :init
  (setq cquery-executable "/usr/bin/cquery")
  (setq cquery-extra-init-params '(:index (:comments 2) :cacheFormat "msgpack"))
  :config
  (add-hook 'c-mode-common-hook 'lsp-cquery-enable))

(use-package company-lsp
  :config
  (push 'company-lsp company-backends)
  (setq company-transformers nil
	company-lsp-async t
	company-lsp-cache-candidates nil))

;; TODO: use (or create) something more generic.
(defun my-cpp-git-compile ()
  "Compile current git project in the \"build\" directory.

Also checks if there is \"compile_commands.json\" file in the project 
root directory. If not, links to the one in \"build\".

TEMPORARY FUNCTION"
  (interactive)
  (when (eq major-mode 'c++-mode)	;check if in c++-mode
    (let* ((project-dir (vc-find-root buffer-file-name ".git")))
      ;; make build directory if it doesn't exist
      (when (not (file-exists-p (concat project-dir "build")))
	(make-directory (concat project-dir "build")))
      ;; run cmake and make from inside build dir
      (compile (concat "cd " project-dir "build && "
		       "cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=YES .. && "
		       "make"))
      ;; check if cquery is active and if compile_commands.json exists
      ;; if both conditions aren't met, create link to compile_commands.json
      ;;    and try to enable cquery afterwards
      (when (not (or lsp--cur-workspace
		     (file-exists-p (concat project-dir
					    "compile_commands.json"))))
	(async-shell-command (concat "ln -s "
			       project-dir "build/compile_commands.json "
			       project-dir "compile_commands.json"))
	(lsp-cquery-enable)))))
(define-key c++-mode-map (kbd "C-c C-c") 'my-cpp-git-compile)
(define-key c++-mode-map (kbd "C-.") 'xref-find-definitions-other-window)
(define-key c++-mode-map (kbd "M-i") 'counsel-imenu)

(use-package meson-mode
  :config
  (my-print-missing-packages-as-warnings "MESON-MODE" '("meson")))

(use-package helm-xref
  :config
  (setq xref-show-xrefs-function 'helm-xref-show-xrefs))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode))

(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  ;; smartparens-strict-mode changes kill commands to omit parens
  ;; TODO: add keybindings for manipulating sexpressions in elisp mode
  (add-hook 'emacs-lisp-mode-hook 'smartparens-strict-mode)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
					      ("* ||\n[i]" "RET"))))

  (global-set-key (kbd "C-M-t") 'sp-transpose-sexp)
  ;; (global-set-key (kbd "M-r") 'sp-raise-sexp) ;replaces parent with the child
  (global-set-key (kbd "M-'") 'sp-rewrap-sexp))

(defun my-wrap-round ()
  "Wrap the following sexp in parentheses."
  (interactive)
  (save-excursion
    (insert "(")
    (forward-sexp)
    (insert ")"))
  (indent-sexp)
  (forward-char))
(global-set-key (kbd "C-(") 'my-wrap-round)

(defun my-kill-hybrid-sexp ()
  "Kill a line respecting delimiters.
Used second time kills the delimiter and everything up to the next delimiter."
  (interactive)
  (if (member (char-to-string (char-after))
	      (loop for (left . right) in sp-pair-list
		    collect right))
      (progn (delete-char 1)
	     (when (not (looking-at "\n"))
	       (sp-kill-hybrid-sexp (point))))
    (sp-kill-hybrid-sexp (point))))
(define-key c-mode-base-map (kbd "C-k") 'my-kill-hybrid-sexp)

(use-package magit
  :init
  (my-print-missing-packages-as-warnings "MAGIT" '("git"))
  ;; set up ssh-agent
  (setenv "SSH_AUTH_SOCK"
	  (concat
	   (getenv "XDG_RUNTIME_DIR")
	   "/ssh-agent.socket"))
  :bind ("C-x g" . magit-status))

(use-package which-key
  ;; According to documentation this should be :config, but it seems
  ;; like these options are reversed
  ;; How do you load mode before package is loaded?
  :init
  (which-key-mode 1)
  (global-unset-key (kbd "C-h C-h"))	;unbind conflicting key binding
  :bind ("C-*" . which-key-show-top-level))

(use-package whole-line-or-region
  :config
  ;; When pressing C-w this will check if there is an active region
  ;; if there's not, it will kill current line (including newline sign)
  (add-hook 'after-init-hook 'whole-line-or-region-mode))

(use-package dockerfile-mode)

;; TODO: Rethink if this is necessary
;; OCTAVE
;; for info check http://wiki.octave.org/Emacs
;; set octave-mode for all .m files
(autoload 'octave-mode "octave-mod" nil t)
(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))
;; turn on the abbrevs, auto-fill and font-lock features automatically
(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

;; ERC
(load "~/.emacs.d/erc-init.el")

;; YAML-MODE
(use-package yaml-mode)

;; LATEX

;; mainly additional navigation for LaTeX
(let ((byte-compile-warnings '(not-free-vars)))
  (use-package latex-extra
    :config
    (add-hook 'LaTeX-mode-hook #'latex-extra-mode)))

;; preview buffer for LaTeX
(use-package latex-preview-pane
  :pin melpa
  :config
  (latex-preview-pane-enable)
  (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode))

;; emacs' notifications.el
(use-package notifications)

;; Ace jump mode for jumping to char
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-char-mode)))

;; Mutliple cursors
;; Documentation: https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
	 ("C->" . mc/mark-next-like-this)
	 ("C-<" . mc/mark-previous-like-this)
	 ("C-c C-<" . mc/mark-all-like-this)))

;; Undo tree
;; C-x u - undo-tree-visualize
;; C-?   - undo-tree-redo
(use-package undo-tree
  :config
  (global-undo-tree-mode 1))

;; PYTHON
;; (use-package company-jedi
;;   ;; python-virtualenv must be installed on a host system
;;   ;; I could automate it by checking and calling pip install
;;   :config
;;   (add-to-list 'company-backends 'company-jedi))
(use-package elpy
  ;; Python packages needed:
  ;;   jedi or rope, flake8, importmagic, autopep8, yapf
  :init
  (my-print-missing-packages-as-warnings
   "ELPY" '("jedi" "flake8" "importmagic" "autopep8" "yapf") t)
  :config
  (elpy-enable))

;; MAIL
(load "~/.emacs.d/mu4e-init.el")

;; password-store
;; https://git.zx2c4.com/password-store/tree/contrib/emacs/README.md
;; For smtp auth check defun smtpmail-try-auth-methods in smtpmail.el
(use-package password-store)

;; elfeed - for rss feeds
(use-package elfeed
  :config
  (defun my-elfeed-open-yt-video ()
    (interactive)
    "Opens a youtube video in the current entry using MPV."
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
	       do (elfeed-untag entry 'unread)
	       when (elfeed-entry-link entry)
	       do (message (shell-command-to-string
			    (concat "i3-msg exec mpv \"" it "\""))))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))
  (defun my-elfeed-open-link () (interactive) (my-elfeed-open-yt-video))
  (define-key elfeed-search-mode-map (kbd "v") 'my-elfeed-open-link)
  (load "~/.emacs.d/elfeed_settings.el") ;overrites my-elfeed-open-link
)

;; lyrics
(use-package lyrics
  :config
  (load "~/.emacs.d/my-lyrics.el"))

;; eww customization

(use-package eww
  :config
  (load "~/.emacs.d/eww-init.el"))

;; RUST
;; (use-package lsp-rust
;;   :config
;;   (setq lsp-rust-rls-command '("rustup" "run" "nightly" "rls"))
;;   (add-hook 'rust-mode-hook #'lsp-rust-enable)
;;   (add-hook 'rust-mode-hook #'flycheck-mode))

(provide 'init)
;;; init.el ends here
