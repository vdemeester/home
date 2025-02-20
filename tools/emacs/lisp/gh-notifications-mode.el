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
    (define-key map (kbd "RET") 'github-notifications-open-at-point)
    map)
  "Keymap for GitHub notifications buffer.")

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
    (let* ((type (or .subject.type "Unknown"))
           (id .id)
           (url .subject.url)
           (repo .repository.full_name)
           (status (propertize "●" 'face 'github-notifications-unread-face))
           (ci-status
            (if (string= type "PullRequest")
                (github-notifications--format-ci-status
                 (github-notifications--get-pr-statuses
                  repo
                  (github-notifications--get-pr-number url)))
              " ")))
      (list id
            (vector
             status
             ci-status
             repo
             type
             (propertize .subject.title
                        'notification-id id
                        'notification-url url
                        'notification-type type
                        'repo repo)
             (github-notifications--format-time .updated_at))))))

(defun github-notifications--get-pr-number (url)
  "Extract pull request number from URL."
  (when (string-match "/pulls/\\([0-9]+\\)" url)
    (match-string 1 url)))

(defun github-notifications-fetch ()
  "Fetch notifications using gh CLI."
  (github-notifications--ensure-gh)
  (let* ((json-string
          (with-temp-buffer
            (unless (= 0 (call-process "gh" nil t nil
                                     "api"
                                     "-H" "Accept: application/vnd.github+json"
                                     "/notifications"))
              (error "Failed to fetch notifications"))
            (buffer-string)))
         (notifications (github-notifications--parse-json json-string)))
    (github-notifications--display notifications)))

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
  (when-let* ((entry (tabulated-list-get-entry))
              (title-col (aref entry 3))
              (id (get-text-property 0 'notification-id title-col)))
    (github-notifications--ensure-gh)
    (with-temp-buffer
      (unless (= 0 (call-process "gh" nil t nil
                                "api"
                                "-X" "PATCH"
                                (format "/notifications/threads/%s" id)))
        (error "Failed to mark notification as read")))
    (github-notifications-refresh)))

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

(defun github-notifications--get-pr-statuses (repo pr-number)
  "Get CI statuses for a PR using the GitHub GraphQL API."
  (when (and repo pr-number)
    (with-temp-buffer
      (call-process "gh" nil t nil
                   "api" "graphql"
                   "-f" (format "query=%s"
                              (github-notifications--make-graphql-query repo pr-number)))
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
             (pendings (seq-count (lambda (s) (string= s "pending")) statuses)))
        (list :total total
              :successes successes
              :failures failures
              :pendings pendings)))))

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
             ((> pendings 0) '(:foreground "yellow"))
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
