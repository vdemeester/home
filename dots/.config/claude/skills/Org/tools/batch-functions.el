;;; batch-functions.el --- Org-mode batch operations -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vincent Demeester

;; Author: Vincent Demeester <vincent@sbr.pm>
;; Keywords: org, batch, automation
;; Version: 1.0.0

;;; Commentary:

;; Elisp functions for batch-mode org-mode file manipulation.
;; Provides read and write operations on org files without GUI.
;; Used by org-manager CLI tool and Claude Code skills.

;;; Code:

(require 'org)
(require 'org-element)
(require 'json)

;;; Configuration

(setq org-todo-keywords
      '((sequence "STRT(s)" "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "CANX(c@/!)")))

(setq org-priority-highest 1
      org-priority-lowest 5
      org-priority-default 4)

;; Silence interactive prompts
(setq org-use-fast-todo-selection nil
      org-log-done nil  ; Will be set per-operation as needed
      org-agenda-inhibit-startup t)

;;; Utility Functions

(defun org-batch--format-timestamp (timestamp)
  "Format TIMESTAMP element to string."
  (when timestamp
    (org-element-property :raw-value timestamp)))

(defun org-batch--priority-to-number (priority-char)
  "Convert PRIORITY-CHAR to number (1-5)."
  (when priority-char
    (- priority-char 48)))  ; ASCII '1' = 49

(defun org-batch--number-to-priority (num)
  "Convert NUM (1-5) to priority character."
  (when (and num (>= num 1) (<= num 5))
    (+ num 48)))  ; Convert to ASCII

(defun org-batch--element-to-alist (element)
  "Convert org ELEMENT to JSON-friendly alist."
  `((heading . ,(org-element-property :raw-value element))
    (todo . ,(org-element-property :todo-keyword element))
    (priority . ,(org-batch--priority-to-number
                  (org-element-property :priority element)))
    (tags . ,(org-element-property :tags element))
    (level . ,(org-element-property :level element))
    (scheduled . ,(org-batch--format-timestamp
                   (org-element-property :scheduled element)))
    (deadline . ,(org-batch--format-timestamp
                  (org-element-property :deadline element)))))

;;; Read Operations

(defun org-batch-list-todos (file &optional filter-state filter-priority filter-tags)
  "List TODOs from FILE with optional filters.
FILTER-STATE: String like \"NEXT\" or \"TODO\"
FILTER-PRIORITY: Number 1-5 or list of numbers
FILTER-Tags: List of tag strings (match any)"
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((todos '())
          (priority-list (if (listp filter-priority)
                             filter-priority
                           (when filter-priority (list filter-priority)))))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((todo (org-element-property :todo-keyword hl))
                (priority (org-batch--priority-to-number
                           (org-element-property :priority hl)))
                (tags (org-element-property :tags hl)))
            (when (and todo
                       ;; State filter
                       (or (null filter-state)
                           (string= todo filter-state))
                       ;; Priority filter
                       (or (null priority-list)
                           (member priority priority-list))
                       ;; Tag filter (match any)
                       (or (null filter-tags)
                           (and tags (seq-intersection filter-tags tags))))
              (push (org-batch--element-to-alist hl) todos)))))
      (nreverse todos))))

(defun org-batch-scheduled-today (file &optional date)
  "Get items scheduled for DATE (default today) from FILE.
DATE should be in format \"YYYY-MM-DD\" or \"today\"."
  (let* ((target-date (if (or (null date) (string= date "today"))
                          (format-time-string "%Y-%m-%d")
                        date))
         (items '()))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((scheduled (org-element-property :scheduled hl)))
            (when scheduled
              (let ((sched-val (org-element-property :raw-value scheduled)))
                (when (string-match target-date sched-val)
                  (push (org-batch--element-to-alist hl) items))))))))
    (nreverse items)))

(defun org-batch-by-section (file section-name)
  "Get all TODOs under SECTION-NAME (level 1 heading) in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((in-section nil)
          (todos '()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((level (org-element-property :level hl))
                (heading (org-element-property :raw-value hl))
                (todo (org-element-property :todo-keyword hl)))
            ;; Track which section we're in
            (when (= level 1)
              (setq in-section (string= heading section-name)))
            ;; Collect TODOs in this section (level > 1)
            (when (and in-section todo (> level 1))
              (push (org-batch--element-to-alist hl) todos)))))
      (nreverse todos))))

(defun org-batch-count-by-state (file)
  "Count TODOs in FILE by state.
Returns alist with counts for each state."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((counts '((total . 0) (TODO . 0) (NEXT . 0) (STRT . 0) (WAIT . 0) (DONE . 0) (CANX . 0))))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((todo (org-element-property :todo-keyword hl)))
            (when todo
              (setcdr (assoc 'total counts) (1+ (cdr (assoc 'total counts))))
              (let ((state-entry (assoc (intern todo) counts)))
                (when state-entry
                  (setcdr state-entry (1+ (cdr state-entry)))))))))
      counts)))

(defun org-batch-search (file search-term)
  "Search for SEARCH-TERM in FILE content.
Returns list of matching headlines with context."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((matches '())
          (search-regexp (regexp-quote search-term)))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let* ((begin (org-element-property :begin hl))
                 (end (org-element-property :end hl))
                 (content (buffer-substring-no-properties begin end)))
            (when (string-match-p search-regexp content)
              (push (cons (cons 'matched-in
                                (if (string-match-p search-regexp
                                                    (org-element-property :raw-value hl))
                                    "heading"
                                  "content"))
                          (org-batch--element-to-alist hl))
                    matches)))))
      (nreverse matches))))

(defun org-batch-get-sections (file)
  "Get list of all level-1 sections in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((sections '()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (when (= 1 (org-element-property :level hl))
            (push (org-element-property :raw-value hl) sections))))
      (nreverse sections))))

(defun org-batch-get-children (file heading-name)
  "Get all direct children TODOs of HEADING-NAME in FILE.
Returns only immediate children (level = parent + 1), not all descendants."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((children '())
          (parent-level nil)
          (in-subtree nil))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((heading (org-element-property :raw-value hl))
                (level (org-element-property :level hl)))
            (cond
             ;; Found the parent heading
             ((string= heading heading-name)
              (setq parent-level level
                    in-subtree t))
             ;; We're past the parent's subtree (same or lower level)
             ((and in-subtree parent-level (<= level parent-level))
              (setq in-subtree nil))
             ;; We're in the subtree and this is a direct child
             ((and in-subtree parent-level (= level (1+ parent-level)))
              (push (org-batch--element-to-alist hl) children))))))
      (nreverse children))))

;;; Write Operations

(defun org-batch-update-state (file heading new-state)
  "Update TODO state for HEADING in FILE to NEW-STATE.
Returns t on success, nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\|DONE\\|CANX\\) \\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (let ((org-log-done (if (string= new-state "DONE") 'time nil)))
          (org-todo new-state))
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-add-todo (file section heading &optional scheduled priority tags)
  "Add new TODO to FILE in SECTION with HEADING.
SCHEDULED: Date string \"YYYY-MM-DD\"
PRIORITY: Number 1-5
TAGS: List of tag strings"
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((section-regexp (concat "^\\* " (regexp-quote section) "$")))
      (if (re-search-forward section-regexp nil t)
          (progn
            ;; Find end of section
            (org-end-of-subtree t)
            ;; Insert new TODO
            (insert "\n** TODO ")
            (when priority
              (insert (format "[#%d] " priority)))
            (insert heading)
            (when tags
              (insert " :" (string-join tags ":") ":"))
            (insert "\n")
            (when scheduled
              (insert (format "SCHEDULED: <%s>\n" scheduled)))
            (insert ":PROPERTIES:\n")
            (insert (format ":CREATED: [%s]\n"
                            (format-time-string "%Y-%m-%d %a %H:%M")))
            (insert ":END:\n")
            (write-region (point-min) (point-max) file)
            t)
        ;; Section not found
        nil))))

(defun org-batch-schedule-task (file heading date)
  "Schedule task with HEADING in FILE for DATE.
DATE should be \"YYYY-MM-DD\" format.
Returns t on success, nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\) \\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (org-schedule nil date)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-set-deadline (file heading date)
  "Set deadline for task with HEADING in FILE to DATE.
DATE should be \"YYYY-MM-DD\" format.
Returns t on success, nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\) \\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (org-deadline nil date)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-set-priority (file heading priority)
  "Set PRIORITY (1-5) for task with HEADING in FILE.
Returns t on success, nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\) \\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading)))
          (priority-char (org-batch--number-to-priority priority)))
      (when (and priority-char (re-search-forward heading-regexp nil t))
        (org-back-to-heading)
        (org-priority priority-char)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-archive-done (file)
  "Archive all DONE and CANX items in FILE.
Returns count of archived items."
  (let ((count 0)
        (archive-location nil))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      ;; Find and archive DONE items
      (goto-char (point-min))
      (while (re-search-forward "^\\*+ \\(DONE\\|CANX\\) " nil t)
        (org-back-to-heading)
        ;; Get archive location from properties if available
        (let ((local-archive (org-entry-get nil "ARCHIVE")))
          (when local-archive
            (setq archive-location local-archive)))
        (condition-case err
            (progn
              (when archive-location
                (let ((org-archive-location archive-location))
                  (org-archive-subtree)))
              (unless archive-location
                (org-archive-subtree))
              (setq count (1+ count)))
          (error
           (message "Failed to archive: %s" (error-message-string err)))))
      ;; Save changes
      (write-region (point-min) (point-max) file))
    count))

;;; Output Functions

(defun org-batch-output-json (success data &optional error)
  "Output JSON response.
SUCCESS: boolean
DATA: data to include in response
ERROR: error message if any"
  (let ((response (if success
                      `((success . ,success) (data . ,data))
                    `((success . :json-false) (error . ,error)))))
    (princ (json-encode response))
    (terpri)))

(defun org-batch-output-error (message)
  "Output error MESSAGE in JSON format."
  (org-batch-output-json nil nil message))

(provide 'batch-functions)
;;; batch-functions.el ends here
