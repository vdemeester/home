;;; programming-lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; LSP configuration
;;; Code:
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (nix-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-eldoc-render-all t))

(use-package consult-lsp
  :after (lsp))

(provide 'programming-lsp)
;;; programming-lsp.el ends here
