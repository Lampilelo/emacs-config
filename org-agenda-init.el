(setq org-directory "~/org")

(defun org-file-path (filename)
  "Return the absolute address of an org file, given its relative name."
  (concat (file-name-as-directory org-directory) filename))

(setq org-inbox-file "~/org/inbox.org")
(setq org-index-file (org-file-path "index.org"))
(setq org-archive-location
      (concat (org-file-path "archive.org") "::* From %s"))

;; Copy things from inbox.org on Dropbox to index.org
;; (defun hrs/copy-tasks-from-inbox ()
;;   (when (file-exists-p org-inbox-file)
;;     (save-excursion
;;       (find-file org-index-file)
;;       (goto-char (point-max))
;;       (insert-file-contents org-inbox-file)
;;       (delete-file org-inbox-file))))

(setq org-agenda-files (list org-index-file))

;; Rebind org-archive-subtree to also mark an item as DONE
(defun hrs/mark-done-and-archive ()
  "Mark the state of an org-mode item as DONE and archive it."
  (interactive)
  (org-todo 'done)
  (org-archive-subtree))

(define-key org-mode-map (kbd "C-c C-x C-s") 'hrs/mark-done-and-archive)

;; Record the time that a todo was archived.
(setq org-log-done 'time)

;; Capture templates for quickly adding notes
(setq org-capture-templates
      '(("t" "Todo"
	 entry
	 (file+headline org-index-file "Inbox")
	 "* TODO %?\nCREATED: %u\n")))

(define-key global-map (kbd "C-c l") 'org-store-link)
(define-key global-map (kbd "C-c a") 'org-agenda)
(define-key global-map (kbd "C-c c") 'org-capture)
