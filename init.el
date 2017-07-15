(require 'package) ;; You might already have this line
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize) ;; You might already have this line

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "#d55e00" "#009e73" "#f8ec59" "#0072b2" "#cc79a7" "#56b4e9" "white"])
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(custom-enabled-themes (quote (monokai)))
 '(custom-safe-themes
   (quote
    ("a49760e39bd7d7876c94ee4bf483760e064002830a63e24c2842a536c6a52756" "f78de13274781fbb6b01afd43327a4535438ebaeec91d93ebdbba1e3fba34d3c" "08b8807d23c290c840bbb14614a83878529359eaba1805618b3be7d61b0b0a32" default)))
 '(font-use-system-font t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(inhibit-startup-screen t)
 '(org-edit-src-content-indentation 2)
 '(org-src-fontify-natively t)
 '(org-startup-indented t)
 '(org-use-extra-keys t)
 '(package-archives
   (quote
    (("melpa" . "https://melpa.org/packages/")
     ("gnu" . "http://elpa.gnu.org/packages/"))))
 '(package-selected-packages
   (quote
    (smartparens which-key nyan-mode speed-type magit monokai-theme sublime-themes whole-line-or-region autopair highlight-parentheses flycheck-irony flycheck-rtags cmake-ide helm-rtags rtags auto-complete company-irony-c-headers company-irony irony yasnippet list-packages-ext helm cmake-mode dart-mode atom-one-dark-theme)))
 '(safe-local-variable-values (quote ((cmake-ide-build-dir . "./build"))))
 '(sentence-end-base "[.?!…‽][]\"'”’)}]*")
 '(sentence-end-double-space nil)
 '(split-width-threshold 140)
 '(split-window-preferred-function (quote split-window-sensibly)))
;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; '(default ((t (:inherit nil :stipple nil :background "#282C34" :foreground "#ABB2BF" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :foundry "PfEd" :family "DejaVu Sans Mono")))))
(set-face-attribute 'default nil :height 130) ; 120 for 80 characters per half the screen
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Privacy and security while downloading packages
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

;; Test the above settings by using the following code snippet:
;;  (let ((bad-hosts
;;         (loop for bad
;;               in `("https://wrong.host.badssl.com/"
;;                    "https://self-signed.badssl.com/")
;;               if (condition-case e
;;                      (url-retrieve
;;                       bad (lambda (retrieved) t))
;;                    (error nil))
;;               collect bad)))
;;    (if bad-hosts
;;        (error (format "tls misconfigured; retrieved %s ok" bad-hosts))
;;      (url-retrieve "https://badssl.com"
;;                    (lambda (retrieved) t))))

;; Custom keybindings
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-i") 'imenu)

;; Set style for c and c++
(setq c-default-style "linux"
      c-basic-offset 4)

;; HELM
(require 'helm-config)

;;==================== AUTO-COMPLETION ====================

;; FLYCHECK
(require 'flycheck)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c-mode-hook 'flycheck-mode)

;; Set flycheck to work with c++17
;; !! It's not working with rtags and irony checkers, because these are
;; !! using compiler flags defined in compile-commands.json
;; better to do that in .dir-locals.el with:
;;   ((c++-mode (flycheck-gcc-language-standard . "c++17")))
;;(add-hook 'c++-mode-hook (lambda () (setq flycheck-gcc-language-standard "c++17")))
;(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))
; (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-pedantic t)))

;; company-mode
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

;; RTAGS
(require 'rtags)
(require 'company-rtags)

(setq rtags-completions-enabled t)
(push 'company-rtags company-backends)
(setq rtags-autostart-diagnostics t)
(rtags-enable-standard-keybindings)

;set rtags bin dir and start rdm process
(setq rtags-path "~/.emacs.d/rtags/build")
;(rtags-start-process-unless-running)

;integration with helm
(setq rtags-display-result-backend 'helm)

;; we can add integration with flycheck with:
(require 'flycheck-rtags)
(defun my-flycheck-rtags-setup ()
  (flycheck-select-checker 'rtags)
  (setq-local flycheck-highlighting-mode nil) ;;RTags creates more accurate overlays
  (setq-local flycheck-check-syntax-automatically nil))
;; c-mode-common-hook is also called by c++-mode
(add-hook 'c-mode-common-hook #'my-flycheck-rtags-setup)

;; Rebind keys for finding symbols and references
(define-key c-mode-map (kbd "C-.") 'rtags-find-symbol-at-point)
(define-key c++-mode-map (kbd "C-.") 'rtags-find-symbol-at-point)
(define-key c-mode-map (kbd "C-,") 'rtags-find-references-at-point)
(define-key c++-mode-map (kbd "C-,") 'rtags-find-references-at-point)

;; IRONY

;; irony-mode
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)

(defun my-irony-mode-hook()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))

(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

;; company-irony
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
;(setq company-backends (delete 'company-semantic company-backends))
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-irony))
;; no delay for tab-completion
(setq company-idle-delay 0)
(require 'cc-mode)
;; (define-key c-mode-map [(tab)] 'company-complete)
;; (define-key c++-mode-map [(tab)] 'company-complete)
;; Define key for force search completion
(define-key c-mode-map (kbd "C-'") 'company-complete)
(define-key c++-mode-map (kbd "C-'") 'company-complete)


(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

;; integrate irony with flycheck
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))

;; YASNIPPET
(require 'yasnippet)
(yas-global-mode 1)
;; Add yasnippet support for all company backends
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend)    (member 'company-yasnippet backend)))
  backend
(append (if (consp backend) backend (list backend))
        '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))


;; activate automatic cmake for rtags completion
(require 'cmake-ide)
(setq cmake-ide-rdm-executable "~/.emacs.d/rtags/build/bin/rdm")
(setq cmake-ide-rdm-rc-path "~/.emacs.d/rtags/build/bin/")
;(setq cmake-ide-flags-c++ "-std=c++1y")
(cmake-ide-setup)

;;================= END OF AUTO-COMPLETION =================

;; Eldoc to show function interface in minibuffer
(defun my-eldoc-hook ()
  (setq-local eldoc-documentation-function #'rtags-eldoc))
(add-hook 'c-mode-common-hook #'my-eldoc-hook)

;; parentheses highlighting
(require 'highlight-parentheses)

;; integrate autopair with highlight-parentheses-mode
;; autopair actually isn't activated
(require 'autopair)
(add-hook 'highlight-parentheses-mode-hook
	  '(lambda ()
	     (setq autopair-handle-action-fns
		   (append
		    (if autopair-handle-action-fns
			autopair-handle-action-fns
		      '(autopair-default-handle-action))
		    '((lambda (action pair pos-before)
			(hl-paren-color-update)))))))

(add-hook 'c-mode-common-hook #'highlight-parentheses-mode)
(add-hook 'emacs-lisp-mode-hook #'highlight-parentheses-mode)


;; smartparens
(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)
;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; org-mode
(add-hook 'org-mode-hook #'visual-line-mode)


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

;; When pressing C-w this will check if there is an active region
;;   if there's not, it will kill current line (including newline sign)
(add-hook 'after-init-hook #'whole-line-or-region-mode)

;; MAGIT
(global-set-key (kbd "C-x g") 'magit-status)

;; Set default browser for opening links
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "vivaldi")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil)))))

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
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;; DOXYMACS
;; It's not entirely functioning because it depends on:
;;   W3 (emacs www browser)
;;   tempo (some script, not shure what it does)
;; Above packages are not installed!
(add-to-list 'load-path "~/.emacs.d/doxymacs-1.8.0/build/lisp/")
(require 'doxymacs)
(add-hook 'c-mode-common-hook 'doxymacs-mode)
(setq doxymacs-external-xml-parser-executable
      "~/.emacs.d/doxymacs-1.8.0/build/c/doxymacs_parser")

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
;; ident
(setq
 erc-nick "Oxon"
 erc-server "irc.rizon.net"
 erc-prompt-for-password nil
 erc-prompt-for-nickserv-password nil)
;;(erc :server "irc.rizon.net" :port 6667 :nick "Oxon")
(load "~/.erc/passwords")
(setq erc-nickserv-passwords
      `((Rizon (("Oxon" . ,rizon-oxon-pass)))))
;; logs
(setq
 erc-log-channels-directory "~/.erc/logs/"
 erc-save-buffer-on-part t)
;; load modules
(defun my-erc-hook()
  (require 'erc-services)
  'erc-services-enable t
  ;; (require 'erc-notifications)
  ;; 'erc-notifications-enable t
  (require 'erc-log)
  'erc-log-enable t)
(add-hook 'erc-mode-hook 'my-erc-hook)
;; auto-join channels
(erc-autojoin-mode 0)
(setq erc-autojoin-channels-alist
      '((".*rizon.*" "#krasnale" "#emurh")))
(add-hook 'erc-server-NOTICE-functions 'my-post-vhost-autojoin)
(defun my-post-vhost-autojoin (proc parsed)
  "Autojoin when NickServ tells us to."
  (with-current-buffer (process-buffer proc)
    (when (string-match ".*Password accepted.*"
                             (erc-response.contents parsed))
      (erc-autojoin-channels erc-session-server (erc-current-nick))
      nil)))
