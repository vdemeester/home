;;; programming-go.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Go programming language configuration
;;; Code:
(use-package go-mode
  :commands (go-mode)
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

(use-package gotest
  :after go-mode)

(use-package gotest-ui
  :commands (gotest-ui-current-test gotest-ui-current-file gotest-ui-current-project)
  :after (go-mode gotest)
  :bind (:map go-mode-map
              ("C-c t t" . gotest-ui-current-test)
              ("C-c t f" . gotest-ui-current-file)
              ("C-c t p" . gotest-ui-current-project)))

(use-package go-stacktracer
  :commands (go-stacktracer-region))

(provide 'programming-go)
;;; programming-go.el ends here
