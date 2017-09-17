;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)
(package-initialize)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))

;; Privacy and security while downloading packages
;; it needs gnutls(-bin) and python-certifi packages to work
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
(setq inhibit-startup-screen t)
;; ido mode
;; TODO: check out https://masteringemacs.org/article/introduction-to-ido-mode
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
;; org mode customizations
(setq org-edit-src-content-indentation 2)
(setq org-src-fontify-natively t)
(setq org-startup-indented t)
(setq org-use-extra-keys t)
(setq org-todo-keywords
      '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))
(setq org-todo-keyword-faces
      '(("IN-PROGRESS" . "yellow1")
	("WAITING" . "gold2")))
;; (add-hook 'org-mode-hook #'visual-line-mode) ;; TODO: check it out
(column-number-mode 1)
(setq split-width-threshold 140)
(setq split-window-preferred-function (quote split-window-sensibly))

;; Transparent emacs
;;                                        (<active> . <inactive)
; (set-frame-parameter (selected-frame) 'alpha '(90 . 90))
; (add-to-list 'default-frame-alist '(alpha . (85 . 50)))

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

;; org-mode source coloring
;; Note 1: python-pygments needs to be installed
(setq org-latex-listings 'minted)
(require 'ox-latex)
(add-to-list 'org-latex-packages-alist '("" "minted"))

(setq org-latex-pdf-process
      '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
	"pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))

;; GDB
(setq gdb-many-windows t
      ;; Display source file containing the main routine at startup
      gdb-show-main t)

;; Custom global keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)

(add-hook 'org-mode-hook 'visual-line-mode)


;; ==================== FUNCTIONS ===================

;; Got it from here: http://www.draketo.de/light/english/emacs/babcore
(defun x-urgency-hint (frame arg &optional source)
  "Set the x-urgency hint for the frame to arg: 

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
  "Mark the current emacs frame as requiring urgent attention. 

With a prefix argument which does not equal a boolean value of nil, remove the urgency flag (which might or might not change display, depending on the window manager)."
  (interactive "P")
  (let (frame (selected-frame))
  (x-urgency-hint frame (not arg))))

;; ==================== PACKAGES ====================

(use-package monokai-theme
  :config
  (load-theme 'monokai t)
  (set-face-attribute 'default nil :height 120))

(use-package auto-complete)

(use-package helm
  :config
  (require 'helm-config))

(use-package flycheck
  :config
  (add-hook 'c++-mode-hook 'flycheck-mode)
  (add-hook 'c-mode-hook 'flycheck-mode))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode))

;;(shell-command )
(use-package rtags
  :init
  (setq rtags-path "~/.emacs.d/rtags/build/bin/")
  ;; helm as backend for displaying
  (setq rtags-display-result-backend 'helm)
  :config
  (rtags-enable-standard-keybindings)
  (if (not (file-exists-p rtags-path))
      (shell-command "sh ~/.emacs.d/get-rtags.sh"))
  (rtags-start-process-unless-running)
  ;; Rebind keys for finding symbols and references
  :bind (:map c-mode-map
	      ("C-." . rtags-find-symbol-at-point)
	      ("C-," . rtags-find-references-at-point)
	 :map c++-mode-map
	      ("C-." . rtags-find-symbol-at-point)
	      ("C-," . rtags-find-references-at-point)))

(use-package company-rtags
  :init
  (setq rtags-completions-enabled t)
  (setq rtags-autostart-diagnostics t)
  :config
  (push 'company-rtags company-backends))

(use-package flycheck-rtags
  :config
  (add-hook 'c-mode-common-hook
	    (lambda ()
	      "Flycheck RTags setup"
	      (flycheck-select-checker 'rtags)
		(message "flycheck-rtags lambda")
	       ;;RTags creates more accurate overlays
	       (setq-local flycheck-highlighting-mode nil) 
	       (setq-local flycheck-check-syntax-automatically nil))))

(use-package helm-rtags)

;; TODO: build server if there's a new version
(use-package irony
  :config
  (if (not (file-exists-p "~/.emacs.d/irony/bin/irony-server"))
      (irony-install-server
       (format
	(concat "%s %s %s && %s --build . "
		"--use-stderr --config Release --target install")
	(shell-quote-argument irony-cmake-executable)
	(shell-quote-argument (concat "-DCMAKE_INSTALL_PREFIX="
				      (expand-file-name
				       irony-server-install-prefix)))
	(shell-quote-argument irony-server-source-dir)
	(shell-quote-argument irony-cmake-executable))))  
  (add-hook 'c-mode-common-hook 'irony-mode)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  :bind (:map irony-mode-map
	      ([remap completion-at-point] .
	       irony-completion-at-point-async)
	      ([remap complete-symbol] .
	       irony-completion-at-point-async)))

(use-package company-irony
  :init
  (setq company-idle-delay 0)
  :config
  (add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
  (add-to-list 'company-backends 'company-irony)
  :bind (:map c-mode-map ("C-'" . company-complete)
	 :map c++-mode-map ("C-'" . company-complete)))

(use-package flycheck-irony)

(use-package company-irony-c-headers
  :config
  (add-to-list 'company-backends
	       '(company-irony-c-headers company-irony))
  (add-hook 'flycheck-mode-hook 'flycheck-irony-setup))

(use-package yasnippet
  :init
  (yas-global-mode 1)
  ;; Add yasnippet support for all company backends
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")
  :config
  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas)
	    (and (listp backend)
		 (member 'company-yasnippet backend)))
	backend
        (append (if (consp backend) backend (list backend))
		'(:with company-yasnippet))))
  (setq company-backends (mapcar #'company-mode/backend-with-yas
				company-backends)))

(use-package cmake-mode)

(use-package cmake-ide
  :init
  (setq cmake-ide-rdm-executable "~/.emacs.d/rtags/build/bin/rdm")
  (setq cmake-ide-rdm-rc-path "~/.emacs.d/rtags/build/bin/"))  

;; Eldoc to show function interface in minibuffer
(defun my-eldoc-hook ()
  (setq-local eldoc-documentation-function #'rtags-eldoc))
(add-hook 'c-mode-common-hook 'my-eldoc-hook)

(use-package highlight-parentheses)

(use-package autopair
  :config
  ;; (add-hook 'highlight-parentheses-mode-hook
  ;; 	    '(lambda ()
  ;; 	       (setq autopair-handle-action-fns
  ;; 		     (append
  ;; 		      (if autopair-handle-action-fns
  ;; 			  autopair-handle-action-fns
  ;; 			'(autopair-default-handle-action))
  ;; 		      '((lambda (action pair pos-before)
  ;; 			  (hl-paren-color-update)))))))
  (add-hook 'c-mode-common-hook 'highlight-parentheses-mode)
  (add-hook 'emacs-lisp-mode-hook 'highlight-parentheses-mode))

;; TODO: check out https://github.com/Fuco1/smartparens/wiki#information-for-new-users
(use-package smartparens
  :config
  (require 'smartparens-config)
  (show-smartparens-global-mode +1)
  (smartparens-global-mode 1)
  (sp-with-modes '(c-mode c++-mode)
    (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
    (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
					      ("* ||\n[i]" "RET")))))

(use-package magit
  :bind ("C-x g" . magit-status))

(use-package which-key
  :config
  (add-hook 'after-init-hook 'which-key-mode))

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

; preview buffer for LaTeX
(use-package latex-preview-pane
  :config
  (latex-preview-pane-enable)
  (add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode))

; emacs' notifications.el
(use-package notifications)

;; Ace jump mode for jumping to char
(use-package ace-jump-mode
  :bind (("C-;" . ace-jump-mode)))

;; Undo tree
;; C-x u - undo-tree-visualize
;; C-?   - undo-tree-redo
(use-package undo-tree)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (latex-preview-pane latex-extra yasnippet yaml-mode whole-line-or-region which-key use-package smartparens monokai-theme magit highlight-parentheses helm-rtags flycheck-rtags flycheck-irony dockerfile-mode company-rtags company-irony-c-headers company-irony cmake-mode cmake-ide autopair auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
