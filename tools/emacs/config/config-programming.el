;;; config-programming.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Configure general programming
;;; Code:

(declare-function vde-project--project-root-or-default-directory "proj-func")

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

(use-package run-command
  :bind ("C-c c" . run-command)
  :config
  ;; TODO (defun run-command-recipe-make ())
  (defun run-command-recipe-hack ()
    "Returns a dynamic list of commands based of the presence of an `hack' folder
in the project root *or* the default-directory."
    (let* ((dir (vde-project--project-root-or-default-directory))
	   (hack-dir (expand-file-name "hack" dir))
	   (files (directory-files hack-dir)))
      (when (file-accessible-directory-p hack-dir)
	(mapcar (lambda (file)
		  (let ((hack-file (expand-file-name file hack-dir)))
		    (when (and (file-regular-p hack-file)
			       (file-executable-p hack-file))
		      (list :command-line hack-file
			    :command-name file
			    :working-dir dir
			    :runner 'run-command-runner-compile))))
		files))))
  (add-to-list 'run-command-recipes 'run-command-recipe-hack))

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
