;;; config-mu4e.el -- mu emacs client configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mu4e
  :commands (mu4e)
  :custom
  (mu4e-context-policy 'pick-first)
  :config

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
  (setq mu4e-contexts `( ,(make-mu4e-context
			   :name "icloud"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (mu4e-trash-folder . "/icloud/Deleted Messages")
				   ;; (mu4e-refile-folder . vde/mu4e-icloud-refile)
				   (mu4e-sent-folder . "/icloud/Sent Messages")
				   (mu4e-draft-folder . "/icloud/Drafts")
				   (mu4e-get-mail-command . "mbsync icloud")
				   ))
			 ;; ,(make-mu4e-context
			 ;;   :name "gmail"
			 ;;   :match-func (lambda (msg) (when msg
			 ;; 			       (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
			 ;;   :vars '(
			 ;; 	   (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
			 ;; 	   (mu4e-refile-folder . "/Gmail/[Gmail].Archive")
			 ;; 	   (mu4e-get-mail-command . "mbsync gmail")
			 ;; 	   ))
			 ))
  (add-to-list 'mu4e-bookmarks
	       '( :name  "All Inboxes"
		  :query "maildir:/icloud/INBOX OR maildir:/gmail/INBOX"
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
