;;; config-vcs.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Version control configuration
;;; Code:


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

(global-set-key (kbd "C-x v B") 'vde/vc-browse-remote)

(use-package vc
  :config
  (setq-default vc-find-revision-no-save t
                vc-follow-symlinks t)
  :bind (("C-x v f" . vc-log-incoming)  ;  git fetch
         ("C-x v F" . vc-update)
         ("C-x v d" . vc-diff)))

(use-package vc-dir
  :config
  (defun vde/vc-dir-project ()
    "Unconditionally display `vc-diff' for the current project."
    (interactive)
    (vc-dir (vc-root-dir)))

  (defun vde/vc-dir-jump ()
    "Jump to present directory in a `vc-dir' buffer."
    (interactive)
    (vc-dir default-directory))
  :bind (("C-x v p" . vde/vc-dir-project)
         ("C-x v j" . vde/vc-dir-jump) ; similar to `dired-jump'
         :map vc-dir-mode-map
         ("f" . vc-log-incoming) ; replaces `vc-dir-find-file' (use RET)
         ("F" . vc-update)       ; symmetric with P: `vc-push'
         ("d" . vc-diff)         ; align with D: `vc-root-diff'
         ("k" . vc-dir-clean-files)))

(use-package vc-git
  :config
  (setq vc-git-diff-switches "--patch-with-stat")
  (setq vc-git-print-log-follow t))

(use-package vc-annotate
  :config
  (setq vc-annotate-display-mode 'scale)
  :bind (("C-x v a" . vc-annotate)
         :map vc-annotate-mode-map
         ("t" . vc-annotate-toggle-annotation-visibility)))

(use-package ediff
  :commands (ediff ediff-files ediff-merge ediff3 ediff-files3 ediff-merge3)
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-diff-options "-w")
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package diff
  :config
  (setq diff-default-read-only nil)
  (setq diff-advance-after-apply-hunk t)
  (setq diff-update-on-the-fly t)
  (setq diff-refine 'font-lock)
  (setq diff-font-lock-prettify nil)
  (setq diff-font-lock-syntax nil))

(use-package magit-popup)

(defun th/magit--with-difftastic (buffer command)
  "Run COMMAND with GIT_EXTERNAL_DIFF=difft then show result in BUFFER."
  (let ((process-environment
         (cons (concat "GIT_EXTERNAL_DIFF=difft --width="
                       (number-to-string (frame-width)))
               process-environment)))
    ;; Clear the result buffer (we might regenerate a diff, e.g., for
    ;; the current changes in our working directory).
    (with-current-buffer buffer
      (setq buffer-read-only nil)
      (erase-buffer))
    ;; Now spawn a process calling the git COMMAND.
    (make-process
     :name (buffer-name buffer)
     :buffer buffer
     :command command
     ;; Don't query for running processes when emacs is quit.
     :noquery t
     ;; Show the result buffer once the process has finished.
     :sentinel (lambda (proc event)
                 (when (eq (process-status proc) 'exit)
                   (with-current-buffer (process-buffer proc)
                     (goto-char (point-min))
                     (ansi-color-apply-on-region (point-min) (point-max))
                     (setq buffer-read-only t)
                     (view-mode)
                     (end-of-line)
                     ;; difftastic diffs are usually 2-column side-by-side,
                     ;; so ensure our window is wide enough.
                     (let ((width (current-column)))
                       (while (zerop (forward-line 1))
                         (end-of-line)
                         (setq width (max (current-column) width)))
                       ;; Add column size of fringes
                       (setq width (+ width
                                      (fringe-columns 'left)
                                      (fringe-columns 'right)))
                       (goto-char (point-min))
                       (pop-to-buffer
                        (current-buffer)
                        `(;; If the buffer is that wide that splitting the frame in
                          ;; two side-by-side windows would result in less than
                          ;; 80 columns left, ensure it's shown at the bottom.
                          ,(when (> 80 (- (frame-width) width))
                             #'display-buffer-at-bottom)
                          (window-width
                           . ,(min width (frame-width))))))))))))
(defun th/magit-show-with-difftastic (rev)
  "Show the result of \"git show REV\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If REV is given, just use it.
          (when (boundp 'rev) rev)
          ;; If not invoked with prefix arg, try to guess the REV from
          ;; point's position.
          (and (not current-prefix-arg)
               (or (magit-thing-at-point 'git-revision t)
                   (magit-branch-or-commit-at-point)))
          ;; Otherwise, query the user.
          (magit-read-branch-or-commit "Revision"))))
  (if (not rev)
      (error "No revision specified")
    (th/magit--with-difftastic
     (get-buffer-create (concat "*git show difftastic " rev "*"))
     (list "git" "--no-pager" "show" "--ext-diff" rev))))
(defun th/magit-diff-with-difftastic (arg)
  "Show the result of \"git diff ARG\" with GIT_EXTERNAL_DIFF=difft."
  (interactive
   (list (or
          ;; If RANGE is given, just use it.
          (when (boundp 'range) range)
          ;; If prefix arg is given, query the user.
          (and current-prefix-arg
               (magit-diff-read-range-or-commit "Range"))
          ;; Otherwise, auto-guess based on position of point, e.g., based on
          ;; if we are in the Staged or Unstaged section.
          (pcase (magit-diff--dwim)
            ('unmerged (error "unmerged is not yet implemented"))
            ('unstaged nil)
            ('staged "--cached")
            (`(stash . ,value) (error "stash is not yet implemented"))
            (`(commit . ,value) (format "%s^..%s" value value))
            ((and range (pred stringp)) range)
            (_ (magit-diff-read-range-or-commit "Range/Commit"))))))
  (let ((name (concat "*git diff difftastic"
                      (if arg (concat " " arg) "")
                      "*")))
    (th/magit--with-difftastic
     (get-buffer-create name)
     `("git" "--no-pager" "diff" "--ext-diff" ,@(when arg (list arg))))))

(use-package magit
  :unless noninteractive
  :commands (magit-status magit-clone magit-pull magit-blame magit-log-buffer-file magit-log)
  :bind (("C-c v c" . magit-commit)
         ("C-c v C" . magit-checkout)
         ("C-c v b" . magit-branch)
         ("C-c v d" . magit-dispatch)
         ("C-c v f" . magit-fetch)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v L" . magit-log)
         ("C-c v p" . magit-pull)
         ("C-c v P" . magit-push)
         ("C-c v r" . magit-rebase)
	 ("C-c v s" . magit-stage)
         ("C-c v v" . magit-status))
  :config
  (transient-define-prefix th/magit-aux-commands ()
    "My personal auxiliary magit commands."
    ["Auxiliary commands"
     ("d" "Difftastic Diff (dwim)" th/magit-diff-with-difftastic)
     ("s" "Difftastic Show" th/magit-show-with-difftastic)])
  (transient-append-suffix 'magit-dispatch "!"
    '("#" "My Magit Cmds" th/magit-aux-commands))
  (setq-default magit-save-repository-buffers 'dontask
                magit-refs-show-commit-count 'all
                magit-branch-prefer-remote-upstream '("main")
		magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
		magit-bury-buffer-function #'magit-restore-window-configuration
		magit-refresh-status-buffer nil)

  (setq-default git-commit-summary-max-length 50
                git-commit-style-convention-checks
                '(non-empty-second-line
                  overlong-summary-line))

;; TODO: complete with list of issues (async ?)
;;   (transient-append-suffix 'git-commit-insert-trailer "t"
;;   '("i" "Issue numero" hello))
;; 
;; (defun hello (foo)
;;   (interactive (list (completing-read "Foo number"
;; 				'("foo" "bar" "baz"))))
;;   (message foo)
;;   (git-commit--insert-trailer "Hello" foo))

  ;; (magit-define-popup-option 'magit-rebase-popup
  ;;                            ?S "Sign using gpg" "--gpg-sign=" #'magit-read-gpg-secret-key)
  (magit-define-popup-switch 'magit-log-popup
                             ?m "Omit merge commits" "--no-merges")
  ;; cargo-culted from https://github.com/magit/magit/issues/3717#issuecomment-734798341
  ;; valid gitlab options are defined in https://docs.gitlab.com/ee/user/project/push_options.html
  ;;
  ;; the second argument to transient-append-suffix is where to append
  ;; to, not sure what -u is, but this works
  (transient-append-suffix 'magit-push "-u"
    '(1 "=s" "Skip gitlab pipeline" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-push "=s"
    '(1 "=m" "Create gitlab merge-request" "--push-option=merge_request.create"))
  (transient-append-suffix 'magit-push "=m"
    '(1 "=o" "Set push option" "--push-option="))  ;; Will prompt, can only set one extra

  (defun vde/fetch-and-rebase-from-upstream ()
    ""
    (interactive)
    (magit-fetch-all "--quiet")
    (magit-git-rebase (concat "upstream/" (vc-git--symbolic-ref (buffer-file-name))) "-sS"))
  
  ;; Hide "Recent Commits"
  (magit-add-section-hook 'magit-status-sections-hook
                          'magit-insert-modules
                          'magit-insert-unpushed-to-upstream
                          'magit-insert-unpulled-from-upstream)
  ;; No need for tag in the status header
  (remove-hook 'magit-status-sections-hook 'magit-insert-tags-header)
  (setq-default magit-module-sections-nested nil)

  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t))

(use-package gitconfig-mode
  :commands (gitconfig-mode)
  :mode (("/\\.gitconfig\\'"  . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/git/config\\'"    . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :commands (gitignore-mode)
  :mode (("/\\.gitignore\\'"        . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'"          . gitignore-mode)))

(use-package gitattributes-mode
  :commands (gitattributes-mode)
  :mode (("/\\.gitattributes" . gitattributes-mode)))

(use-package dired-git-info
  :disabled
  :bind (:map dired-mode-map
              (")" . dired-git-info-mode))
  :defer 2)

(defun git-blame-line ()
  "Runs `git blame` on the current line and
   adds the commit id to the kill ring"
  (interactive)
  (let* ((line-number (save-excursion
                        (goto-char (point-at-bol))
                        (+ 1 (count-lines 1 (point)))))
         (line-arg (format "%d,%d" line-number line-number))
         (commit-buf (generate-new-buffer "*git-blame-line-commit*")))
    (call-process "git" nil commit-buf nil
                  "blame" (buffer-file-name) "-L" line-arg)
    (let* ((commit-id (with-current-buffer commit-buf
                        (buffer-substring 1 9)))
           (log-buf (generate-new-buffer "*git-blame-line-log*")))
      (kill-new commit-id)
      (call-process "git" nil log-buf nil
                    "log" "-1" "--pretty=%h   %an   %s" commit-id)
      (with-current-buffer log-buf
        (message "Line %d: %s" line-number (buffer-string)))
      (kill-buffer log-buf))
    (kill-buffer commit-buf)))

(use-package diff-hl
  :hook (find-file . diff-hl-mode)
  :hook (prog-mode . diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :bind
  (:map diff-hl-command-map
	("n" . diff-hl-next-hunk)
	("p" . diff-hl-previous-hunk)
	("[" . nil)
	("]" . nil)
	("DEL"   . diff-hl-revert-hunk)
	("<delete>" . diff-hl-revert-hunk)
	("SPC" . diff-hl-mark-hunk)
	:map vc-prefix-map
	("n" . diff-hl-next-hunk)
	("p" . diff-hl-previous-hunk)
	("s" . diff-hl-stage-dwim)
	("DEL"   . diff-hl-revert-hunk)
	("<delete>" . diff-hl-revert-hunk)
	("SPC" . diff-hl-mark-hunk))
  :config
  (put 'diff-hl-inline-popup-hide
       'repeat-map 'diff-hl-command-map))

(use-package diff-hl-inline-popup
  :after (diff-hl))
(use-package diff-hl-show-hunk
  :after (diff-hl))

(use-package diff-hl-dired
  :after (diff-hl)
  :hook (dired-mode . diff-hl-dired-mode))

(use-package consult-vc-modified-files
  :after consult
  :bind
  ("C-x v /" . consult-vc-modified-files))

;; FIXME bind pr-review-submit-review
(use-package pr-review
  :commands (pr-review pr-review-open pr-review-submit-review)
;;  :bind
;;  (("M-<SPC> p r" . pr-review-submit-review))
  :custom
  (pr-review-ghub-host "api.github.com")
  (pr-review-notification-include-read nil)
  (pr-review-notification-include-unsubscribed nil))

(use-package pr-review-search
  :commands (pr-review-search pr-review-search-open pr-review-current-repository pr-review-current-repository-search)
;;  :bind
;;  (("M-<SPC> p a" . pr-review-current-repository)
;;   ;; FIXME understand why this one doesn't work
  ;;  ("M-<SPC> p s" . pr-review-current-repository-search)))
  )

(use-package pr-review-notification
  :commands (pr-review-notification)
;;  :bind
  ;;  (("M-<SPC> p n" . pr-review-notification)))
  )

(defun pr-review-current-repository-search (query)
  "Run pr-review-search on the current repository."
  (interactive "sSearch query: ")
  (pr-review-search (format "is:pr archived:false is:open repo:%s %s" (vde/gh-get-current-repo) query)))

(defun pr-review-current-repository ()
  "Run pr-review-search on the current repository."
  (interactive)
  (pr-review-search (format "is:pr archived:false is:open repo:%s" (vde/gh-get-current-repo))))

;; (pr-review-search "build is:pr archive:false is:open repo:tektoncd/pipeline")
;; (pr-review-search "build")

;; TODO this is relatively slow. Cache result or ?
(defun vde/gh-get-current-repo ()
  "Get the current repository name using the `gh' command line."
  (unless (executable-find "gh")
    (error "GitHub CLI (gh) command not found"))

  (with-temp-buffer
    (let ((exit-code (call-process "gh" nil t nil "repo" "view" "--json" "owner,name" "--template" "{{.owner.login}}/{{.name}}")))
      (unless (= exit-code 0)
	(error "Failed to get repository info: gh command exited with code %d" exit-code))
      (string-trim (buffer-string)))))


(provide 'config-vcs)
;;; config-vcs.el ends here
