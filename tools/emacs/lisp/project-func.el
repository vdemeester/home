;;; project-func.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'project)
(require 'json)
(require 'vc)

(defun in-git-repo-p ()
  "Check if current directory is in a git repository."
  (eq (vc-backend (or buffer-file-name default-directory))
      'Git))

(defun is-github-repo-p ()
  "Check if current git repository has a GitHub remote."
  (when (in-git-repo-p)
    (string-match-p "github\\.com"
                    (shell-command-to-string "git remote -v"))))

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
  (cond
   ((not (in-git-repo-p))
    (message "Not in a Git repository"))
   ((not (is-github-repo-p))
    (message "Not a GitHub repository"))
   (t
    (let* ((prs (fetch-github-prs))
           (candidates (format-pr-candidates prs))
           (selected (if candidates
                         (cdr (assoc (completing-read "Checkout PR: " candidates)
                                    candidates))
                       nil)))
      (if selected
          (shell-command (format "gh pr checkout %d" selected))
        (message "No pull requests found"))))))

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
