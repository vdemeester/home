;;; programming-treesitter.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Treesitter configuration
;;; Code:

(use-package treesit-auto
  :config
  (setq treesit-auto-install 'prompt)
  (global-treesit-auto-mode))

(provide 'programming-treesitter)
;;; programming-treesitter.el ends here
