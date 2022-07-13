;;; config-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Project related configuration.
;;; Code:

(use-package project
  :bind (("C-x p v" . vde-project-magit-status)
         ("C-x p s" . vde-project-vterm))
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
          (?s "Vterm" vde-project-vterm)))

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
    (let* ((pr (project-current t))
           (dir (cdr pr)))
      (magit-status dir)))

  (defun vde-project-vterm ()
    "Run `vterm' on project.
If a buffer already exists for running a vterm shell in the project's root,
switch to it. Otherwise, create a new vterm shell."
    (interactive)
    (let* ((default-directory (project-root (project-current t)))
           (default-project-vterm-name (project-prefixed-buffer-name "vterm"))
           (vterm-buffer (get-buffer default-project-vterm-name)))
      (if (and vterm-buffer (not current-prefix-arg))
          (pop-to-buffer-same-window vterm-buffer)
        (let* ((cd-cmd (concat " cd " (shell-quote-argument default-directory))))
          (vterm default-project-vterm-name)
          (with-current-buffer vterm-buffer
            (vterm-send-string cd-cmd)
            (vterm-send-return)))))))

(provide 'config-projects)
;;; config-projects.el ends here
