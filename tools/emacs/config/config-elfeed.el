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
  (use-package elfeed-org
    :config
    (setq-default rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-notes-dir)))))

(provide 'config-elfeed)
;;; config-elfeed.el ends here
