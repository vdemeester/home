;;; programming-lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; LSP configuration
;;; Code:
(use-package lsp-mode
  :unless noninteractive
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-eldoc-render-all t)
  (add-hook 'python-mode-hook #'lsp)
  (add-hook 'go-mode-hook     #'lsp)
  (setq lsp-eldoc-render-all t))

(use-package consult-lsp
  :after (lsp))

(provide 'programming-lsp)
;;; programming-lsp.el ends here
