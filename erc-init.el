;; ==================== ERC ====================
;; ident
(setq
 erc-nick "Oxon"
 erc-server "irc.rizon.net"
 erc-prompt-for-password nil
 erc-prompt-for-nickserv-password nil)
;;(erc :server "irc.rizon.net" :port 6667 :nick "Oxon")
(add-hook 'erc-mode-hook 'erc-nickserv-mode)

(if (file-exists-p "~/.erc/passwords")
    ;; then
    (progn
      (load "~/.erc/passwords")
      (setq erc-nickserv-passwords
	    `((Rizon (("Oxon" . ,rizon-oxon-pass))))))
  ;; else
  (message "ERC config: file ~/.etc/passwords NOT FOUND"))

;; logs
(require 'erc-log)
(erc-log-enable)
(setq erc-log-channels-directory "~/.erc/logs/")
(setq erc-save-buffer-on-part t)
;; load modules
(require 'erc-services)
(setq erc-services-enable t)
;; (require 'erc-notifications)
;; 'erc-notifications-enable t

;; auto-join channels
(erc-autojoin-mode 0)
(setq erc-autojoin-channels-alist
      '((".*rizon.*" "#krasnale")))
(add-hook 'erc-server-NOTICE-functions 'my-post-vhost-autojoin)
(defun my-post-vhost-autojoin (proc parsed)
  "Autojoin when NickServ tells us to."
  (with-current-buffer (process-buffer proc)
    (when (string-match ".*Password accepted.*"
                             (erc-response.contents parsed))
      (erc-autojoin-channels erc-session-server (erc-current-nick))
      nil)))
;; misc options
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)
 ;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; =============================================
