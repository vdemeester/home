;;; programming-go.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Go programming language configuration
;;; Code:


(use-package gotest
  :commands (my-gotest-maybe-ts-run go-test--get-current-test-info)
  :after go-mode
  :custom
  (go-test-verbose t)
  :hook
  (go-test-mode . (lambda () (pop-to-buffer (get-buffer "*Go Test*"))))
  (go-mode . (lambda ()(interactive) (setq go-run-args "-v")))
  :config
  (defun my-go-test-current-project()
    (interactive)
    (let ((default-directory (project-root (project-current t))))
      (go-test-current-project)))
  (defun my-gotest-maybe-ts-run()
    (interactive)
    (let ((testrunname)
          (gotest (cadr (go-test--get-current-test-info))))
      (save-excursion
        (goto-char (line-beginning-position))
        (re-search-forward "name:[[:blank:]]*\"\\([^\"]*\\)\"" (line-end-position) t))
      (setq testrunname (match-string-no-properties 1))
      (if testrunname
          (setq gotest (format "%s/%s" gotest (shell-quote-argument
                                               (replace-regexp-in-string " " "_" testrunname)))))
      (go-test--go-test (concat "-run " gotest "\\$ .")))))

(use-package go-ts-mode
  :mode (("\\.go$" . go-ts-mode)
         ("\\.go" . go-ts-mode)
         ("\\.go\\'" . go-ts-mode)))

(provide 'programming-go)
;;; programming-go.el ends here
