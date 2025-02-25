;;; project-func.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'project)
(require 'json)

(defun fetch-github-prs ()
  "Fetch GitHub PRs synchronously."
  (let* ((output (shell-command-to-string "gh pr list --limit=5000 --json number,title,author,url,baseRefName,labels"))
         (prs (json-read-from-string output)))
    prs))

(defun format-pr-candidates (prs)
  "Format PR data into candidates for completion."
  (mapcar (lambda (pr)
            (let-alist pr
	      (cons (format "#%d %s (by @%s) on %s" .number .title .author.login .baseRefName)
                    .number)))
          prs))

;;;###autoload
(defun checkout-github-pr ()
  "Interactive function to select and checkout a GitHub PR."
  (interactive)
  (let* ((prs (fetch-github-prs))
         (candidates (format-pr-candidates prs))
         (selected (cdr (assoc (completing-read "Checkout PR: " candidates)
			       candidates))))
    (when selected
      (shell-command (format "gh pr checkout %d" selected)))))

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

(provide 'project-func)
;;; project-func.el ends here
