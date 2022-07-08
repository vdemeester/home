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
  (use-package elfeed-org
    :command (elfeed-org)
    :config
    (setq-default rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-private-notes-dir)))))

(provide 'config-elfeed)
;;; config-elfeed.el ends here
