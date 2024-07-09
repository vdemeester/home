;;; config-programming.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configure general programming
;;; Code:

(defun my-recompile (args)
  (interactive "P")
  (cond
   ((eq major-mode #'emacs-lisp-mode)
    (call-interactively 'eros-eval-defun))
   ((bound-and-true-p my-vterm-command)
    (my-vterm-execute-region-or-current-line my-vterm-command))
   ((get-buffer "*compilation*")
    (with-current-buffer"*compilation*"
      (recompile)))
   ((get-buffer "*Go Test*")
    (with-current-buffer "*Go Test*"
      (recompile)))
   ((and (eq major-mode #'go-mode)
         buffer-file-name
         (string-match
          "_test\\'" (file-name-sans-extension buffer-file-name)))
    (my-gotest-maybe-ts-run))
   ((and (get-buffer "*cargo-test*")
         (boundp 'my-rustic-current-test-compile)
         my-rustic-current-test-compile)
    (with-current-buffer "*cargo-test*"
      (rustic-cargo-test-run my-rustic-current-test-compile)))
   ((get-buffer "*cargo-run*")
    (with-current-buffer "*cargo-run*"
      (rustic-cargo-run-rerun)))
   ((get-buffer "*pytest*")
    (with-current-buffer "*pytest*"
      (recompile)))
   ((eq major-mode #'python-mode)
    (compile (concat python-shell-interpreter " " (buffer-file-name))))
   ((call-interactively 'compile))))

;; try out consult-gh
;; (use-package consult-gh
;;   :after consult
;;   :config
;;   (add-to-list 'consult-gh-default-orgs-list "vdemeester")
;;   (setq consult-gh-default-orgs-list (append consult-gh-default-orgs-list (remove "" (split-string (or (consult-gh--command-to-string "org" "list") "") "\n"))))
;;   (require 'consult-gh-embark)
;;   (require 'consult-gh-transient)
;;   (setq consult-gh-show-preview t)
;;   (setq consult-gh-preview-key "M-o"))

(provide 'config-programming)
;;; config-programming.el ends here
