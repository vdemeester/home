;;; programming-lsp.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; LSP configuration
;;; Code:
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook ((python-mode . lsp)
         (go-mode . lsp)
         (nix-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq )
  :custom
  ;; turn this on to capture client/server comms before
  ;; submitting bug reports with `lsp-workspace-show-log`
  (lsp-completion-provider :none)
  (lsp-log-io nil)
  ;; (lsp-lens-enable t)
  (lsp-eldoc-enable-hover t)
  ;; (lsp-enable-indentation nil)
  (lsp-prefer-flymake t)
  (lsp-ui-sideline-enable nil)
  (lsp-ui-doc-enable t)
  (lsp-ui-imenu-enable t)
  (lsp-ui-sideline-ignore-duplicate t)
  (lsp-enable-folding t)
  (lsp-enable-dap-auto-configure nil) ; Don't try to auto-enable dap: this creates a lot of binding clashes
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-idle-delay .01)
  (lsp-keymap-prefix nil)
  (lsp-eldoc-render-all nil)
  (lsp-file-watch-threshold 4000)
  (lsp-gopls-complete-unimported t t)
  (lsp-yaml-format-enable t)
  (lsp-gopls-staticcheck t t))

(use-package consult-lsp
  :after (lsp))

(provide 'programming-lsp)
;;; programming-lsp.el ends here
