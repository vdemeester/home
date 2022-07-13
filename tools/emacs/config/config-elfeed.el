;;; config-elfeed.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Elfeed configuration
;;; Code:

(use-package elfeed
  :commands (elfeed)
  :bind (("C-c x e" . elfeed))
  :config
  (setq-default elfeed-log-level 'debug
                elfeed-use-curl 't
                elfeed-db-directory "~/sync/elfeed/db/"
                elfeed-db-index "~/sync/elfeed/index")
  (elfeed-org)
  (defun vde/org-elfeed-entry-store-link ()
    (when elfeed-show-entry
      (let* ((link (elfeed-entry-link elfeed-show-entry))
             (title (elfeed-entry-title elfeed-show-entry)))
        (org-store-link-props
         :link link
         :description title)
        )))
  (add-hook 'org-store-link-functions
            'vde/org-elfeed-entry-store-link)

  (defun elfeed-link-title (entry)
    "Copy the entry title and URL as org link to the clipboard."
    (interactive)
    (let* ((link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (titlelink (org-make-link-string link title))))
    (when titlelink
      (kill-new titlelink)
      (x-set-selection 'PRIMARY titlelink)
      (message "Yanked: %s" titlelink)))
  (defun elfeed-show-link-title ()
    "Copy the current entry title and URL as org link to the clipboard."
    (interactive)
    (elfeed-link-title elfeed-show-entry))

  (defun elfeed-show-quick-url-note ()
    "Fastest way to capture entry link to org agenda from elfeed show mode"
    (interactive)
    (elfeed-link-title elfeed-show-entry)
    (org-capture nil "n")
    (yank)
    (org-capture-finalize))
  
  (defface elfeed-face-tag-reddit
    '((t :foreground "#ffb9a0"))
    "This is a custom font face for the F1 tag in Elfeed.")
  
  (push '(f1 elfeed-face-tag-reddit)
        elfeed-search-face-alist)

  (defun yt-dl-it (url)
    "Downloads the URL in an async shell"
    (let ((default-directory "~/desktop/videos"))
      (async-shell-command (format "youtube-dl \"%s\"" url))))

  (defun elfeed-youtube-dl (&optional use-generic-p)
    "Youtube-DL link"
    (interactive "P")
    (let ((entries (elfeed-search-selected)))
      (cl-loop for entry in entries
               do (elfeed-untag entry 'unread)
               when (elfeed-entry-link entry)
               do (yt-dl-it it))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (define-key elfeed-search-mode-map (kbd "d") 'elfeed-youtube-dl)
  
  (use-package elfeed-org
    :commands (elfeed-org)
    :config
    (setq-default rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-private-notes-dir)))))

(provide 'config-elfeed)
;;; config-elfeed.el ends here
