(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(my-print-missing-packages-as-warnings "MU4E" '("mu"))
(require 'mu4e)

(global-set-key (kbd "C-x m") 'mu4e-compose-new)
(setq mu4e-compose-context-policy 'pick-first)

(setq mu4e-change-filenames-when-moving t)
(setq mu4e-maildir "~/Mail")
(setq mu4e-contexts
      `( ,(make-mu4e-context
	   :name "Gmail"
	   :match-func (lambda (msg) (when msg
				       (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
	   :vars '(
		   (mu4e-trash-folder . "/gmail/[Gmail].Bin")
		   (mu4e-refile-folder . "/gmail/[Gmail].Archive")))
	 ,(make-mu4e-context
	   :name "Yahoo"
	   :match-func (lambda (msg) (when msg
				       (string-prefix-p "/yahoo" (mu4e-message-field msg :maildir))))
	   :vars '(
	   	   (mu4e-trash-folder . "/yahoo/Trash")
	   	   (mu4e-refile-folder . "yahoo/Archive"))
	   )
	 ))

(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
	(concat
	 "flag:unread AND NOT flag:trashed "
	 "AND NOT 'maildir:/gmail/[Gmail].Spam'")
	)
  :config
  (mu4e-alert-enable-mode-line-display)
  (defun my-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display))
  (run-with-timer 0 300 'my-refresh-mu4e-alert-mode-line))

;; I have my "default" parameters from Gmail
(setq mu4e-sent-folder "/sent"
      ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-drafts-folder "/drafts"
      user-mail-address "cubex7@gmail.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Now I set a list of 
(defvar my-mu4e-account-alist
  '(("Gmail"
     (mu4e-sent-folder "/gmail/sent")
     (user-mail-address "cubex7@gmail.com")
     (smtpmail-smtp-user "cubex7")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587)
     )
    ("Yahoo"
     (mu4e-sent-folder "/yahoo/sent")
     (user-mail-address "jakub.wojciech@ymail.com")
     (smtpmail-smtp-user "jakub.wojciech@ymail.com")
     (smtpmail-local-domain "ymail.com")
     (smtpmail-default-smtp-server "smtp.mail.yahoo.com")
     (smtpmail-smtp-server "smtp.mail.yahoo.com")
     (smtpmail-smtp-service 587)
     )
     ;; Include any other accounts here ...
    ))

(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from: 
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
             (mapconcat #'(lambda (var) (car var))
            my-mu4e-account-alist "/"))
           (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
           nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
        account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)

;; Fixing issue with not trashing mail in remote maildir.
;; (defun remove-nth-element (nth list)
;;   (if (zerop nth) (cdr list)
;;     (let ((last (nthcdr (1- nth) list)))
;;       (setcdr last (cddr last))
;;       list)))
;; (setq mu4e-marks (remove-nth-element 5 mu4e-marks))
;; (add-to-list 'mu4e-marks
;;      '(trash
;;        :char ("d" . "▼")
;;        :prompt "dtrash"
;;        :dyn-target (lambda (target msg) (mu4e-get-trash-folder msg))
;;        :action (lambda (docid msg target) 
;;                  (mu4e~proc-move docid
;;                     (mu4e~mark-check-target target) "-N"))))

;; Additional settings
;; ;; Include a bookmark to open all of my inboxes
;; (add-to-list 'mu4e-bookmarks
;;        (make-mu4e-bookmark
;;         :name "All Inboxes"
;;         :query "maildir:/Exchange/INBOX OR maildir:/Gmail/INBOX"
;;         :key ?i))

;; ;; This allows me to use 'helm' to select mailboxes
;; (setq mu4e-completing-read-function 'completing-read)
;; ;; Why would I want to leave my message open after I've sent it?
;; (setq message-kill-buffer-on-exit t)
;; Don't ask for a 'context' upon opening mu4e
(setq mu4e-context-policy 'pick-first)
;; Don't ask to quit... why is this the default?
(setq mu4e-confirm-quit nil)

