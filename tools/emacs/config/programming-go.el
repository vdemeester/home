;;; -*- lexical-binding: t; -*-
(use-package go-mode
  :disabled
  :mode "\\.go$"
  :interpreter "go"
  :config
                                        ;(setq gofmt-command "goimports")
  (if (not (executable-find "goimports"))
      (warn "go-mode: couldn't find goimports; no code formatting/fixed imports on save")
    (add-hook 'before-save-hook 'gofmt-before-save))
  (if (not (string-match "go" compile-command))   ; set compile command default
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet")))

(use-package company-go
  :after (go-mode company)
  :config
  (setq company-go-show-annotation t)
  (push 'company-go company-backends))

(use-package flycheck-golangci-lint
  :disabled
  :hook (go-mode . flycheck-golangci-lint-setup)
  :config (setq flycheck-golangci-lint-tests t))

(use-package gotest
  :disabled
  :after go-mode)

(use-package gotest-ui
  :disabled
  :after (go-mode gotest)
  :bind (:map go-mode-map
              ("C-c t t" . gotest-ui-current-test)
              ("C-c t f" . gotest-ui-current-file)
              ("C-c t p" . gotest-ui-current-project)))

(provide 'setup-go)
