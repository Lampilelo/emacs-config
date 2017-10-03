(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e")
(my-print-missing-packages-as-warnings "MU4E" '("mu"))
(require 'mu4e)


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
	 "flag:unread maildir:/gmail/Inbox "
	 "OR "
	 "flag:unread maildir:/yahoo/Inbox")
	)
  :config
  (mu4e-alert-enable-mode-line-display)
  (defun my-refresh-mu4e-alert-mode-line ()
    (interactive)
    (mu4e~proc-kill)
    (mu4e-alert-enable-mode-line-display))
  (run-with-timer 0 300 'my-refresh-mu4e-alert-mode-line))
