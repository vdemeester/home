;;; config-elfeed.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Elfeed configuration
;;; Code:

(use-package elfeed
  :commands (elfeed))

(use-package elfeed-org
  :after (org elfeed)
  :config
  (elfeed-org)
  (setq-default rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-notes-dir))))

(provide 'config-elfeed)
;;; config-elfeed.el ends here
