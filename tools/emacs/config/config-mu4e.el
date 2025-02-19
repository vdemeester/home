;;; config-mu4e.el -- mu emacs client configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mu4e
  :commands (mu4e)
  :custom
  (mu4e-context-policy 'pick-first)
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachment-dir "~/desktop/downloads")
  :config
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " -a"))
  (setq mu4e-update-interval 1800) ; 30m
  (defun vde-mu4e--refile (msg)
    "Refile function to smartly move `MSG' to a given folder."
    (cond
     ;; FIXME
     ((string= (plist-get (car-safe (mu4e-message-field msg :cc)) :email) "ci_activity@noreply.github.com")
      "/icloud/Deleted Messages")
     (t
      (let ((year (format-time-string "%Y" (mu4e-message-field msg :date))))
	(format "/icloud/Archives/%s" year)))))

  (setq
   mu4e-headers-draft-mark     '("D" . "💈")
   mu4e-headers-flagged-mark   '("F" . "📍")
   mu4e-headers-new-mark       '("N" . "🔥")
   mu4e-headers-passed-mark    '("P" . "❯")
   mu4e-headers-replied-mark   '("R" . "❮")
   mu4e-headers-seen-mark      '("S" . "☑")
   mu4e-headers-trashed-mark   '("T" . "💀")
   mu4e-headers-attach-mark    '("a" . "📎")
   mu4e-headers-encrypted-mark '("x" . "🔒")
   mu4e-headers-signed-mark    '("s" . "🔑")
   mu4e-headers-unread-mark    '("u" . "⎕")
   mu4e-headers-list-mark      '("l" . "🔈")
   mu4e-headers-personal-mark  '("p" . "👨")
   mu4e-headers-calendar-mark  '("c" . "📅"))

  (setopt mu4e-completing-read-function completing-read-function)
  (setq mu4e-refile-folder 'vde-mu4e--refile)
  (setq mu4e-contexts `( ,(make-mu4e-context
			   :name "icloud"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (mu4e-trash-folder . "/icloud/Deleted Messages")
				   (mu4e-sent-folder . "/icloud/Sent Messages")
				   (mu4e-draft-folder . "/icloud/Drafts")
				   ;; (mu4e-get-mail-command . "mbsync icloud")
				   ))
			 ,(make-mu4e-context
			   :name "gmail"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (mu4e-drafts-folder  . "/gmail/[Gmail]/Drafts")
				   (mu4e-sent-folder  . "/gmail/[Gmail]/Sent Mail")
				   ;; (mu4e-refile-folder  . "/gmail/[Gmail]/All Mail")
				   (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")
				   ;; (mu4e-get-mail-command . "mbsync gmail")
				   ))
			 ,(make-mu4e-context
			   :name "redhat"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/redhat" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (mu4e-drafts-folder  . "/redhat/[Gmail]/Drafts")
				   (mu4e-sent-folder  . "/redhat/[Gmail]/Sent Mail")
				   ;; (mu4e-refile-folder  . "/redhat/[Gmail]/All Mail")
				   (mu4e-trash-folder  . "/redhat/[Gmail]/Trash")
				   ;; (mu4e-get-mail-command . "mbsync redhat")
				   ))
			 ))
  (add-to-list 'mu4e-bookmarks
	       '( :name  "All Inboxes"
		  :query "maildir:/icloud/INBOX OR maildir:/gmail/INBOX OR maildir:/redhat/INBOX"
		  :key   ?b))
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

(setq sendmail-program "msmtp"
      send-mail-function 'smtpmail-send-it
      message-sendmail-f-is-evil t
      message-sendmail-extra-arguments '("--read-envelope-from")
      message-send-mail-function 'message-send-mail-with-sendmail)

(provide 'config-mu4e)
;;; config-mu4e.el ends here
