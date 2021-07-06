;;; config-elfeed.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Elfeed configuration
;;; Code:

(use-package elfeed
  :commands (elfeed)
  :config
  (setq-default elfeed-log-level 'debug
                elfeed-use-curl 't)
  (elfeed-org)
  (use-package elfeed-org
    :config
    (setq-default rmh-elfeed-org-files (list (expand-file-name "feeds.org" org-notes-dir)))))

(provide 'config-elfeed)
;;; config-elfeed.el ends here
