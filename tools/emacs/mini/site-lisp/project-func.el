;;; project-func.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'project)
(require 'vterm)
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

;;;##autoload
(defun vde/project-run-in-vterm (command &optional directory)
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

;;;###autoload
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

;;;###autoload
(defun vde/project-try-local (dir)
  "Determine if DIR is a non-VC project."
  (if-let ((root (if (listp vde/project-local-identifier)
                     (seq-some (lambda (n)
                                 (locate-dominating-file dir n))
                               vde/project-local-identifier)
                   (locate-dominating-file dir vde/project-local-identifier))))
      (cons 'local root)))

;;;###autoload
(defun vde/project-vterm (&optional command)
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

;;;###autoload
(defun vde/project-eat ()
  "Run Eat term in the current project's root directory.
If a buffer already exists for running Eshell in the project's root,
switch to it.  Otherwise, create a new Eshell buffer.
With \\[universal-argument] prefix arg, create a new Eshell buffer even
if one already exists."
  (interactive)
  (defvar eat-buffer-name)
  (let* ((default-directory (project-root (project-current t)))
	 (eat-buffer-name (project-prefixed-buffer-name "eat"))
	 (eat-buffer (get-buffer eat-buffer-name)))
    (if (and eat-buffer (not current-prefix-arg))
	(pop-to-buffer eat-buffer (bound-and-true-p display-comint-buffer-action))
      (eat shell-file-name))))

;;;###autoload
(defun vde/project-magit-status ()
  "Run `magit-status' on project."
  (interactive)
  (magit-status (vde-project--project-current)))

(provide 'project-func)
;;; project-func.el ends here
