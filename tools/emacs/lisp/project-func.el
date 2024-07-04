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

(provide 'project-func)
;;; project-func.el ends here
