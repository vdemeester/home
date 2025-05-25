;;; vde-vcs --- vcs related functions -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vincent Demeester
;; Author: Vincent Demeester <vincent@sbr.pm>

;; This file is NOT part of GNU Emacs.
;;; Commentary:

;;; Code:

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
