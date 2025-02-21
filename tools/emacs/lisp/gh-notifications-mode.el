;;; github-notifications-gh.el --- Display GitHub notifications in Emacs using gh CLI -*- lexical-binding: t -*-

;; Author: Your Name
;; Version: 1.0
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:
;; This package provides functionality to fetch and display GitHub notifications
;; in a dedicated buffer using the GitHub CLI (gh) and tabulated-list-mode.
;; Make sure you have gh installed and authenticated before using this package.

;;; For pull-request, fetch the diff from the diff_url field
;;; For CI status, use the statuses_url

;;; Code:

(require 'json)
(require 'tabulated-list)
(require 'diff-mode)

(defgroup github-notifications nil
  "GitHub notifications in Emacs."
  :group 'applications)

(defcustom github-notifications-refresh-interval 300
  "Number of seconds between automatic refresh of notifications."
  :type 'integer
  :group 'github-notifications)

(defface github-notifications-unread-face
  '((t :weight bold))
  "Face for unread notifications."
  :group 'github-notifications)

(defvar github-notifications-buffer-name "*GitHub Notifications*"
  "Name of the buffer for displaying GitHub notifications.")

(defvar github-notifications-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") 'github-notifications-refresh)
    (define-key map (kbd "m") 'github-notifications-mark-read)
    (define-key map (kbd "c") 'github-notifications-comment-on-pr)
    (define-key map (kbd "d") 'github-notifications-mark-done)
    (define-key map (kbd "a") 'github-notifications-approve-pr)
    (define-key map (kbd "w") 'github-notifications-copy-url)
    (define-key map (kbd "r") 'github-notifications-request-changes-on-pr)
    (define-key map (kbd "v") 'github-notifications-show-details)
    (define-key map (kbd "RET") 'github-notifications-open-at-point)
    ;; m for mark
    map)
  "Keymap for GitHub notifications buffer.")

(defvar github-notifications-detail-buffer-name "*GitHub Notification Detail*"
  "Name of the buffer for displaying GitHub notification details.")

(defvar github-notifications-detail-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "q") 'quit-window)
    map)
  "Keymap for GitHub notification detail buffer.")

(define-derived-mode github-notifications-detail-mode special-mode "GitHub-Notification-Detail"
  "Major mode for displaying GitHub notification details."
  (setq buffer-read-only t))

(defvar github-notifications--process-buffer "*github-notifications-process*"
  "Buffer for GitHub notifications processes.")

(defun github-notifications--call-process-async (callback _buf name &rest args)
  "Call gh process asynchronously with ARGS and CALLBACK when done.
Creates a temporary buffer for the process output."
  (let ((temp-buffer (generate-new-buffer (format " *gh-%s*" name))))
    (setenv "TERM" "dumb")
    (setenv "CLICOLOR" "0")
    (setenv "PAGER" "cat")
    (make-process
     :name name
     :buffer temp-buffer
     :command (cons "gh" args)
     :sentinel (lambda (process _event)
                 (unwind-protect
                     (when (eq (process-status process) 'exit)
                       (if (= (process-exit-status process) 0)
                           (with-current-buffer (process-buffer process)
                             (funcall callback (buffer-string)))
                         (message "GitHub CLI process failed")))
                   (kill-buffer temp-buffer))))))

(define-derived-mode github-notifications-mode tabulated-list-mode "GitHub-Notifications"
  "Major mode for displaying GitHub notifications."
  (setq tabulated-list-format
        [("Status" 6 t)
         ("CI" 8 t)
         ("Repository" 30 t)
         ("Type" 15 t)
         ("Title" 60 t)
         ("Updated" 20 t)])
  (setq tabulated-list-sort-key '("Updated" . t))
  (setq tabulated-list-padding 2)
  (tabulated-list-init-header))

(defun github-notifications--ensure-gh ()
  "Ensure gh command line tool is available."
  (unless (executable-find "gh")
    (error "GitHub CLI (gh) not found. Please install it first")))

(defun github-notifications--parse-json (json-string)
  "Parse JSON-STRING into Lisp objects."
  (json-read-from-string json-string))

(defun github-notifications--format-time (time-string)
  "Format TIME-STRING to a human-readable format."
  (format-time-string
   "%Y-%m-%d %H:%M"
   (date-to-time time-string)))

(defun github-notifications--format-list-entry (notification)
  "Format a NOTIFICATION as a tabulated list entry."
  (let-alist notification
    (list .id
          (vector
           (github-notifications--format-unread .unread)
           (if (string= .type "PullRequest")
	       (github-notifications--format-ci-status .ci_status)
	     " ")
	   ;; (if (string= .type "PullRequest")
	   ;; 	 (or .ci_status "?")
           ;;   "")
           .repo
           .type
           (propertize .title
                       'notification-id .id
                       'notification-url .url  ; Add this line
                       'github-notifications--copy-url .url
                       'notification-type .type
                       'repo .repo)
           (github-notifications--format-time .updated)))))

(defun github-notifications-fetch ()
  "Fetch notifications using gh CLI asynchronously."
  (github-notifications--ensure-gh)
  (github-notifications--call-process-async
   (lambda (output)
     (let* ((raw-notifications (github-notifications--parse-json output))
            (normalized-notifications
             (mapcar #'github-notifications--normalize-notification raw-notifications)))
       ;; For each PR notification, fetch its status
       (dolist (notif normalized-notifications)
         (when (and (equal (alist-get 'type notif) "PullRequest"))
           (github-notifications--get-pr-statuses notif))
	 ;; FIXME there is a bug here, it share the same thing
         (github-notifications--display normalized-notifications))))
   (get-buffer-create github-notifications--process-buffer)
   "github-notifications"
   "api"
   "-H" "Accept: application/vnd.github+json"
   "/notifications?all=true"))

(defun github-notifications--normalize-notification (notif)
  "Convert raw notification to normalized format."
  (let-alist notif
    `((id . ,.id)
      (type . ,.subject.type)
      (title . ,.subject.title)
      (repo . ,.repository.full_name)
      (url . ,.subject.url)
      (updated . ,.updated_at)
      (unread . ,.unread)
      (ci_status . nil)
      (statuses_url . ,.subject.statuses_url))))

(defun github-notifications--display (notifications)
  "Display NOTIFICATIONS in the buffer."
  (with-current-buffer (get-buffer-create github-notifications-buffer-name)
    (github-notifications-mode)
    (setq tabulated-list-entries
          (mapcar #'github-notifications--format-list-entry notifications))
    (tabulated-list-print t)))

(defun github-notifications-refresh ()
  "Refresh the notifications buffer."
  (interactive)
  (github-notifications-fetch))

(defun github-notifications-mark-read ()
  "Mark notification at point as read."
  (interactive)
  (when-let* ((entry (github-notifications--get-entry-data))
              (id (nth 4 entry)))
    (github-notifications--ensure-gh)
    (with-temp-buffer
      (unless (= 0 (call-process "gh" nil t nil
                                 "api"
                                 "-X" "PATCH"
                                 (format "/notifications/threads/%s" id)))
        (error "Failed to mark notification as read")))
    (github-notifications-refresh)))

(defun github-notifications-mark-done ()
  "Mark notification at point as done."
  (interactive)
  (when-let* ((entry (github-notifications--get-entry-data))
              (id (nth 4 entry)))
    (github-notifications--ensure-gh)
    (with-temp-buffer
      (unless (= 0 (call-process "gh" nil t nil
                                 "api"
                                 "-X" "DELETE"
                                 (format "/notifications/threads/%s" id)))
        (error "Failed to mark notification as done")))
    (github-notifications-refresh)))

(defun github-notifications--copy-url ()
  "Copy the URL of the notification item at point."
  (interactive)
  (let* ((entry (github-notifications--get-entry-data))
	 (api-url (nth 3 entry))
	 (url (replace-regexp-in-string
               "api\\.github\\.com/repos"
               "github.com"
               (replace-regexp-in-string
                "/pulls/"
                "/pull/"
                api-url))))
    (kill-new url)))

(defun github-notifications-open-at-point ()
  "Open the notification at point in a web browser."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (title-col (aref entry 3))
              (url (get-text-property 0 'notification-url title-col)))
    (let ((web-url (replace-regexp-in-string
                    "api\\.github\\.com/repos"
                    "github.com"
                    (replace-regexp-in-string
                     "/pulls/"
                     "/pull/"
                     url))))
      (browse-url web-url))))

(defun github-notifications--get-entry-data ()
  "Get PR data from current entry."
  (let* ((entry (tabulated-list-get-entry))
	 (id (aref entry 3))
         (title-cell (aref entry 4))
         (repo (get-text-property 0 'repo title-cell))
         (url (get-text-property 0 'notification-url title-cell))
         (notification-id (get-text-property 0 'notification-id title-cell))
         (pr-number (github-notifications--get-pr-number url)))
    (list id repo pr-number url notification-id)))

(defun github-notifications-comment-on-pr ()
  "Add a comment to the pull request at point."
  (interactive)
  (let* ((pr-data (github-notifications--get-entry-data))
         (repo (nth 1 pr-data))
         (pr-number (nth 2 pr-data))
         (comment (read-string "Comment: "))),
    (when (and repo pr-number (not (string-empty-p comment)))
      (let ((default-directory (make-temp-file "gh-pr" t)))
        (call-process "gh" nil "*github-notifications process*" nil
                      "pr" "comment"
		      pr-number
                      "--body" comment
		      "--repo" repo)
        (message "Comment posted successfully")))))

(defun github-notifications-request-changes-on-pr ()
  "Add a comment to the pull request at point."
  (interactive)
  (let* ((pr-data (github-notifications--get-entry-data))
         (repo (nth 1 pr-data))
         (pr-number (nth 2 pr-data))
         (comment (read-string "Comment: ")))
    (when (and repo pr-number (not (string-empty-p comment)))
      (let ((default-directory (make-temp-file "gh-pr" t)))
        (call-process "gh" nil "*github-notifications process*" nil
                      "pr" "review"
		      pr-number
                      "--body" comment
		      "--request-changes"
		      "--repo" repo)
        (message "Comment posted successfully")))))

(defun github-notifications-approve-pr (&optional comment)
  "Approve the pull request at point with an optional comment."
  (interactive
   (list (read-string "Approval comment (optional): ")))
  (let* ((pr-data (github-notifications--get-entry-data))
         (repo (nth 1 pr-data))
         (pr-number (nth 2 pr-data))
         (args (list "pr" "review"
                     pr-number
                     "--approve"
		     "--repo" repo)))
    (when (and repo pr-number)
      (when (and comment (not (string-empty-p comment)))
        (setq args (append args (list "--body" comment))))
      (let ((default-directory (make-temp-file "gh-pr" t)))
        (apply #'call-process "gh" nil "*github-notifications process*" nil args)
        (message "PR approved successfully")))))

(defun github-notifications--get-pr-statuses (notification)
  "Get CI statuses for a PR using the GitHub GraphQL API."
  (let* ((url (alist-get 'url notification))
         (repo (alist-get 'repo notification))
	 (pr-number(github-notifications--get-pr-number url)))
    (when (and repo pr-number)
      (with-temp-buffer
	(github-notifications--call-process-async
	 (lambda (output)
	   (let* ((response (github-notifications--parse-json (buffer-string)))
		  (contexts (thread-last response
					 (alist-get 'data)
					 (alist-get 'repository)
					 (alist-get 'pullRequest)
					 (alist-get 'commits)
					 (alist-get 'nodes)
					 (seq-first)
					 (alist-get 'commit)
					 (alist-get 'statusCheckRollup)
					 (alist-get 'contexts)
					 (alist-get 'nodes)))
		  (statuses (seq-map
			     (lambda (ctx)
                               (let ((state (or (alist-get 'state ctx)
						(alist-get 'conclusion ctx))))
				 (cond
				  ((member state '("SUCCESS" "success" "COMPLETED")) "success")
				  ((member state '("FAILURE" "failure" "ERROR" "error")) "failure")
				  (t "pending"))))
			     contexts))
		  (total (length statuses))
		  (successes (seq-count (lambda (s) (string= s "success")) statuses))
		  (failures (seq-count (lambda (s) (string= s "failure")) statuses))
		  (pendings (seq-count (lambda (s) (string= s "pending")) statuses))
		  (ci-status (list :total total
				   :successes successes
				   :failures failures
				   :pendings pendings)))
	     (setf (alist-get 'ci_status notification) ci-status)))
	 (get-buffer-create (format "*github-notifications-%s-%s-process*" repo pr-number))
	 (format "github-notifications-%s-%s" repo pr-number)
	 "api" "graphql" "-f"
	 (format "query=%s"
		 (github-notifications--make-graphql-query repo pr-number)))))))

(defun github-notifications--format-ci-status (statuses)
  "Format CI status with appropriate face and count information."
  (if (null statuses)
      (propertize "?" 'face '(:foreground "gray"))
    (let* ((total (plist-get statuses :total))
           (successes (plist-get statuses :successes))
           (failures (plist-get statuses :failures))
           (pendings (plist-get statuses :pendings))
           (indicator
            (cond
             ((> failures 0) "✗")
             ((> pendings 0) "○")
             ((= successes total) "✓")
             (t "?")))
           (face
            (cond
             ((> failures 0) '(:foreground "red"))
             ((> pendings 0) '(:foreground "orange"))
             ((= successes total) '(:foreground "green"))
             (t '(:foreground "gray"))))
           (count-str (format "%d/%d" successes total)))
      (concat
       (propertize indicator 'face face)
       " "
       (propertize count-str 'face face)))))

(defun github-notifications--make-graphql-query (repo pr-number)
  "Create a GraphQL query for PR status checks."
  (format "query {
    repository(owner: \"%s\", name: \"%s\") {
      pullRequest(number: %s) {
        commits(last: 1) {
          nodes {
            commit {
              statusCheckRollup {
                state
                contexts(first: 100) {
                  nodes {
                    ... on StatusContext {
                      state
                      context
                    }
                    ... on CheckRun {
                      status
                      conclusion
                      name
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }"
          (car (split-string repo "/"))
          (cadr (split-string repo "/"))
          pr-number))

(defun github-notifications--format-unread (unread)
  "Return a propertized string to showcase the status of the notifications"
  ;; (cond ((unread) (propertize "●" 'face 'github-notifications-unread-face))
  ;; 	((not unread) (propertize "○" 'face 'github-notifications-unread-face))
  ;; 	(t (propertize "?" 'face 'github-notifications-unread-face)))
  (cond (unread (propertize "●" 'face 'github-notifications-unread-face))
	((not unread) (propertize "○" 'face 'github-notifications-unread-face))
	(t (propertize "?" 'face 'github-notifications-unread-face))))

(defun github-notifications--get-pr-number (url)
  "Extract pull request number from URL."
  (when (string-match "/pulls/\\([0-9]+\\)" url)
    (match-string 1 url)))

(defun github-notifications-show-details ()
  "Show detailed view of the notification at point."
  (interactive)
  (when-let* ((entry (tabulated-list-get-entry))
              (title-cell (aref entry 4))
              (type (get-text-property 0 'notification-type title-cell))
              (repo (get-text-property 0 'repo title-cell))
              (url (get-text-property 0 'github-notifications--copy-url title-cell)))
    (let ((buffer (get-buffer-create github-notifications-detail-buffer-name)))
      (with-current-buffer buffer
        (let ((inhibit-read-only t))
          (erase-buffer)
          (github-notifications-detail-mode)
          (cond
           ((string= type "PullRequest")
            (github-notifications--show-pr-details repo url))
           ((string= type "Issue")
            (github-notifications--show-issue-details repo url))
           (t
            (insert "No detailed view available for this notification type."))))
        (goto-char (point-min)))
      (display-buffer buffer))))

(defun github-notifications--show-pr-details (repo url)
  "Show pull request details for REPO and URL."
  (let ((pr-number (github-notifications--get-pr-number url)))
    (when pr-number
      (insert (format "=== Pull Request #%s ===\n\n" pr-number))
      ;; Fetch PR details
      (github-notifications--call-process-async
       (lambda (output)
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char (point-min))
             (forward-line 2)
             (let* ((pr-data (github-notifications--parse-json output)))
               (insert (format "Title: %s\n" (alist-get 'title pr-data)))
               (insert (format "State: %s\n" (alist-get 'state pr-data)))
               (insert (format "\nDescription:\n%s\n" (alist-get 'body pr-data)))))))
       nil
       "pr-details"
       "pr" "view" pr-number "--json" "title,body,state" "--repo" repo)
      
      ;; Fetch and display diff
      (insert "\n=== Diff ===\n\n")
      (github-notifications--call-process-async
       (lambda (output)
         (let ((inhibit-read-only t))
           (save-excursion
             (goto-char (point-max))
             (insert output)
             (let ((diff-start (save-excursion
                                 (goto-char (point-min))
                                 (search-forward "\n=== Diff ===\n\n" nil t))))
               (when diff-start
                 (diff-mode-setup))))))
       nil
       "pr-diff"
       "pr" "diff" pr-number "--repo" repo))))

(defun github-notifications--get-issue-number (url)
  "Extract issue number from URL."
  (when (string-match "/issues/\\([0-9]+\\)" url)
    (match-string 1 url)))

(defun github-notifications--format-checks (checks)
  "Format checks data for display."
  (mapconcat
   (lambda (check)
     (let-alist check
       (format "%s: %s"
               .name
               (propertize .status 'face
                           (pcase .status
                             ("success" '(:foreground "green"))
                             ("failure" '(:foreground "red"))
                             (_ '(:foreground "yellow")))))))
   checks "\n"))

;;;###autoload
(defun github-notifications ()
  "Display GitHub notifications in a buffer."
  (interactive)
  (github-notifications--ensure-gh)
  (let ((buffer (get-buffer-create github-notifications-buffer-name)))
    (with-current-buffer buffer
      (github-notifications-fetch))
    (switch-to-buffer buffer)))

(provide 'github-notifications-gh)
;;; github-notifications-gh.el ends here
