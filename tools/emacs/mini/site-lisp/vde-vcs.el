;;; vde-vcs --- vcs related functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vincent Demeester
;; Author: Vincent Demeester <vincent@sbr.pm>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

;;;###autoload
(defun vde/vc-browse-remote (&optional current-line)
  "Open the repository's remote URL in the browser.
If CURRENT-LINE is non-nil, point to the current branch, file, and line.
Otherwise, open the repository's main page."
  (interactive "P")
  (let* ((remote-url (string-trim (vc-git--run-command-string nil "config" "--get" "remote.origin.url")))
	 (branch (string-trim (vc-git--run-command-string nil "rev-parse" "--abbrev-ref" "HEAD")))
	 (file (string-trim (file-relative-name (buffer-file-name) (vc-root-dir))))
	 (line (line-number-at-pos)))
    (message "Opening remote on browser: %s" remote-url)
    (if (and remote-url (string-match "\\(?:git@\\|https://\\)\\([^:/]+\\)[:/]\\(.+?\\)\\(?:\\.git\\)?$" remote-url))
	(let ((host (match-string 1 remote-url))
	      (path (match-string 2 remote-url)))
	  ;; Convert SSH URLs to HTTPS (e.g., git@github.com:user/repo.git -> https://github.com/user/repo)
	  (when (string-prefix-p "git@" host)
	    (setq host (replace-regexp-in-string "^git@" "" host)))
	  ;; Construct the appropriate URL based on CURRENT-LINE
	  (browse-url
	   (if current-line
	       (format "https://%s/%s/blob/%s/%s#L%d" host path branch file line)
	     (format "https://%s/%s" host path))))
      (message "Could not determine repository URL"))))

;;;###autoload
(defun vde/gh-get-current-repo ()
  "Get the current repository name using the `gh' command line."
  (unless (executable-find "gh")
    (error "GitHub CLI (gh) command not found"))

  (with-temp-buffer
    (let ((exit-code (call-process "gh" nil t nil "repo" "view" "--json" "owner,name" "--template" "{{.owner.login}}/{{.name}}")))
      (unless (= exit-code 0)
	(error "Failed to get repository info: gh command exited with code %d" exit-code))
      (string-trim (buffer-string)))))


(provide 'vde-vcs)
;;; vde-vcs.el ends here
