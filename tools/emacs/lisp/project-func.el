;;; project-func.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'project)

;;;###autoload
(defun vde-project--project-current ()
  "Return directory from `project-current' based on Emacs version."
  (if (>= emacs-major-version 29)
      (project-root (project-current))
    (cdr (project-current))))

;;;###autoload
(defun vde-project--project-root-or-default-directory ()
  "Return path to the project root *or* the default-directory."
  (cond
   ((and (featurep 'project) (project-current))
    (project-root (project-current)))
   (t default-directory)))

;;;##autoload
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

(provide 'project-func)
;;; project-func.el ends here
