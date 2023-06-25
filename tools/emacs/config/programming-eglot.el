;;; programming-eglot.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Eglot configuration
;;; Code:
(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c a" . eglot-code-actions)
        ("C-c r" . eglot-reconnect)
        ("<f2>" . eglot-rename)
        ("C-c ?" . eldoc-print-current-symbol-info))
  :config
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  (add-to-list 'eglot-server-programs `(json-mode  "vscode-json-language-server" "--stdio"))
  (setq-default eglot-workspace-configuration
		'(:gopls (:usePlaceholders t)))
  (setq-default
   eglot-workspace-configuration
   '((:gopls . ((gofumpt . t)))))
  :hook
  (before-save . gofmt-before-save)
  (before-save . eglot-format-buffer)
  (rust-mode . eglot-ensure)
  (sh-script-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (json-mode . eglot-ensure)
  (yaml-mode . eglot-ensure)
  (c-mode . eglot-ensure)
  (cc-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  :custom
  rustic-lsp-client 'eglot)


(provide 'programming-eglot)
;;; programming-eglot.el ends here
