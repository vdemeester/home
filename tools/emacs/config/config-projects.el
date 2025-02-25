;;; config-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Project related configuration.
;;; Code:

(require 'json)

(defun vde/open-readme ()
  "Open a README file in the current project.
It will search for README.org, README.md or README in that order"
  (interactive)
  (let* ((default-directory (vde-project--project-current)))
    (cond ((file-exists-p (expand-file-name "README.org" default-directory))
	   (find-file "README.org"))
	  ((file-exists-p (expand-file-name "README.md" default-directory))
	   (find-file "README.md"))
	  ((file-exists-p (expand-file-name "README" default-directory))
	   (find-file "README")))))

(use-package project
  :bind (("C-x p v" . vde-project-magit-status)
         ("C-x p s" . vde-project-vterm)
         ("C-x p X" . vde-project-run-in-vterm)
	 ("C-x p G" . checkout-github-pr))
  :config
  (setq vde/project-local-identifier '(".project")) ;; "go.mod"
  (setq project-switch-commands
        '((?f "File" project-find-file)
          (?g "Grep" project-find-regexp)
          (?d "Dired" project-dired)
          (?b "Buffer" project-switch-to-buffer)
          (?q "Query replace" project-query-replace-regexp)
          (?m "Magit" vde-project-magit-status)
          (?e "Eshell" project-eshell)
          (?s "Vterm" vde-project-vterm)
	  (?R "README" vde/open-readme)
	  (?g "Checkout GitHub PR" checkout-github-pr)))

  (defun vde-project-run-in-vterm (command &optional directory)
    "Run the given `COMMAND' in a new vterm buffer in `project-root' or the
given `DIRECTORY'.

This is similar to `compile' but with vterm.
One reason for this is to be able to run commands that needs a TTY."
    (interactive "sCommand: ")
    (let* ((cwd (or directory (vde-project--project-root-or-default-directory)))
	   (default-directory cwd)
	   (buffer-name (format "*vterm %s: %s*" cwd command))
           (buffer (get-buffer buffer-name))
           (vterm-kill-buffer-on-exit nil)
	   (vterm-shell (concat "bash -c '" command ";exit'")))
      (when buffer
	(kill-buffer buffer))
      (let ((buffer (generate-new-buffer buffer-name)))
	(pop-to-buffer buffer)
	(with-current-buffer buffer
          (vterm-mode)))))

  (defun vde/project-try-local (dir)
    "Determine if DIR is a non-VC project."
    (if-let ((root (if (listp vde/project-local-identifier)
                       (seq-some (lambda (n)
                                   (locate-dominating-file dir n))
                                 vde/project-local-identifier)
                     (locate-dominating-file dir vde/project-local-identifier))))
        (cons 'local root)))
  (cl-defmethod project-root ((project (head local)))
    (cdr project))

  (cl-defmethod project-root ((project (eql nil))) nil)

  (add-hook 'project-find-functions #'vde/project-try-local)

  :init
  (setq-default project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (defun vde-project-magit-status ()
    "Run `magit-status' on project."
    (interactive)
    (magit-status (vde-project--project-current)))

  (defun vde-project-vterm (&optional command)
    "Run `vterm' on project.
If a buffer already exists for running a vterm shell in the project's root,
switch to it. Otherwise, create a new vterm shell."
    (interactive)
    (let* ((default-directory (vde-project--project-current))
           (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer default-project-vterm-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer-same-window vterm-buffer)
        (let* ((cd-cmd (concat " cd " (shell-quote-argument default-directory))))
          (vterm default-project-vterm-name)
          (with-current-buffer vterm-buffer
            (vterm-send-string cd-cmd)
            (vterm-send-return))))
      (when command
        (vterm-send-string command)
        (vterm-send-return))))
  )

(use-package conner
  :bind (("C-x p C" . conner-run-project-command))
  :init
  (require 'vterm))

(use-package project-x
  :after project
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90)
  (add-hook 'kill-emacs-hook 'project-x--window-state-write)
  (add-to-list 'project-switch-commands
               '(?j "Restore windows" project-x-windows) t)
  :bind (("C-x p w" . project-x-window-state-save)
         ("C-x p j" . project-x-window-state-load)))

(provide 'config-projects)
;;; config-projects.el ends here
