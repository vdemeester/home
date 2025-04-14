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
  
  (defun vde-mu4e--mark-get-copy-target ()
    "Ask for a copy target, and propose to create it if it does not exist."
    (let* ((target (mu4e-ask-maildir "Copy message to: "))
           (target (if (string= (substring target 0 1) "/")
                       target
                     (concat "/" target)))
           (fulltarget (mu4e-join-paths (mu4e-root-maildir) target)))
      (when (mu4e-create-maildir-maybe fulltarget)
	target)))

  (defun copy-message-to-target(docid msg target)
    (let (
          (new_msg_path nil) ;; local variable                                                                
          (msg_flags (mu4e-message-field msg :flags))                                                                                                       
          )                                                                                                   
      ;; 1. target is already determined interactively when executing the mark (:ask-target)                     

      ;; 2. Determine the path for the new file: we use mu4e~draft-message-filename-construct from            
      ;; mu4e-draft.el to create a new random filename, and append the original's msg_flags                   
      (setq new_msg_path (format "%s/%s/cur/%s" mu4e-maildir target (mu4e~draft-message-filename-construct    
								     (mu4e-flags-to-string msg_flags))))                                                                     

      ;; 3. Copy the message using file system call (copy-file) to new_msg_path:                              
      ;; (See e.g. mu4e-draft.el > mu4e-draft-open > resend)                                             
      (copy-file (mu4e-message-field msg :path) new_msg_path)                                                 

      ;; 4. Add the information to the database (may need to update current search query with 'g' if duplicating to current box. Try also 'V' to toggle the display of duplicates) 
      (mu4e~proc-add new_msg_path (mu4e~mark-check-target target))                                              
      )                                                                                                       
    )

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
				   (user-mail-address . "vincent@demeester.fr")
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
				   (user-mail-address . "vinc.demeester@gmail.com")
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
				   (user-mail-address . "vdemeest@redhat.com")
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

(use-package consult-mu
  :after mu4e
  :custom
  ;;maximum number of results shown in minibuffer
  (consult-mu-maxnum 200)
  ;;show preview when pressing any keys
  (consult-mu-preview-key 'any)
  ;;do not mark email as read when previewed. If you turn this to t, be aware that the auto-loaded preview if the preview-key above is 'any would also get marked as read!
  (consult-mu-mark-previewed-as-read nil)
  ;;mark email as read when selected.
  (consult-mu-mark-viewed-as-read t)
  ;;use reply to all when composing reply emails
  (consult-mu-use-wide-reply t)
  ;; define a template for headers view in minibuffer. The example below adjusts the width based on the width of the screen.
  (consult-mu-headers-template (lambda () (concat "%f" (number-to-string (floor (* (frame-width) 0.15))) "%s" (number-to-string (floor (* (frame-width) 0.5))) "%d13" "%g" "%x")))

  :config
  ;;create a list of saved searches for quick access using `histroy-next-element' with `M-n' in minibuffer. Note the "#" character at the beginning of each query! Change these according to
  (setq consult-mu-saved-searches-dynamics '("#flag:unread"))
  (setq consult-mu-saved-searches-async '("#flag:unread"))
  ;; require embark actions for marking, replying, forwarding, etc. directly from minibuffer
  (require 'consult-mu-embark)
  ;; require extra module for composing (e.g. for interactive attachment) as well as embark actions
  (require 'consult-mu-compose)
  (require 'consult-mu-compose-embark)
  ;; require extra module for searching contacts and runing embark actions on contacts
  (require 'consult-mu-contacts)
  (require 'consult-mu-contacts-embark)
  ;; change the prefiew key for compose so you don't open a preview of every file when selecting files to attach
  (setq consult-mu-compose-preview-key "M-o")
  ;; pick a key to bind to consult-mu-compose-attach in embark-file-map
  (setq consult-mu-embark-attach-file-key "C-a")
  (setq consult-mu-contacts-ignore-list '("^.*no.*reply.*"))
  (setq consult-mu-contacts-ignore-case-fold-search t)
  (consult-mu-compose-embark-bind-attach-file-key)
  ;; choose if you want to use dired for attaching files (choice of 'always, 'in-dired, or nil)
  (setq consult-mu-compose-use-dired-attachment 'in-dired))

(provide 'config-mu4e)
;;; config-mu4e.el ends here
