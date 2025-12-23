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

(setq org-priority-highest ?1  ; Highest priority (character '1' = ASCII 49)
      org-priority-lowest ?5   ; Lowest priority (character '5' = ASCII 53)
      org-priority-default ?4) ; Default priority (character '4' = ASCII 52)

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
  "Convert PRIORITY-CHAR to number (1-5).
Priority '1'=1, '2'=2, '3'=3, '4'=4, '5'=5."
  (when priority-char
    (- priority-char 48)))  ; '1'(49) → 1, '2'(50) → 2, ..., '5'(53) → 5

(defun org-batch--number-to-priority (num)
  "Convert NUM (1-5) to priority character.
1='1', 2='2', 3='3', 4='4', 5='5'."
  (when (and num (>= num 1) (<= num 5))
    (+ num 48)))  ; 1 → '1'(49), 2 → '2'(50), ..., 5 → '5'(53)

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

(defun org-batch-get-todo-content (file heading-name)
  "Get full content of TODO with HEADING-NAME in FILE.
Returns alist with metadata, properties, and body content.
Returns nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((found nil)
          (result nil))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (when (and (not found)
                     (string= (org-element-property :raw-value hl) heading-name))
            (setq found t)
            ;; Build result with metadata
            (let* ((basic-data (org-batch--element-to-alist hl))
                   (properties (org-batch--extract-properties hl))
                   (content (org-batch--extract-content hl)))
              (setq result (append basic-data
                                   (list (cons 'properties properties)
                                         (cons 'content content))))))))
      result)))

(defun org-batch--extract-properties (element)
  "Extract properties drawer from ELEMENT as alist."
  (let ((properties '())
        (begin (org-element-property :begin element))
        (end (org-element-property :contents-end element)))
    (when (and begin end)
      (save-excursion
        (goto-char begin)
        (forward-line 1)  ; Skip heading line
        ;; Look for :PROPERTIES: drawer
        (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" end t)
          (let ((drawer-start (point)))
            (when (re-search-forward "^[ \t]*:END:[ \t]*$" end t)
              (let ((drawer-end (match-beginning 0)))
                (goto-char drawer-start)
                ;; Extract each property
                (while (re-search-forward "^[ \t]*:\\([^:\n]+\\):[ \t]*\\(.*\\)$" drawer-end t)
                  (let ((key (match-string 1))
                        (value (match-string 2)))
                    (push (cons key value) properties)))))))))
    (nreverse properties)))

(defun org-batch--extract-content (element)
  "Extract body content from ELEMENT (excluding properties drawer).
Returns the text content without the heading line or properties."
  (let ((end (org-element-property :contents-end element))
        (contents-begin (org-element-property :contents-begin element)))
    (if (and contents-begin end)
        (save-excursion
          (let ((content-text (buffer-substring-no-properties contents-begin end)))
            ;; Remove properties drawer if present
            (with-temp-buffer
              (insert content-text)
              (goto-char (point-min))
              ;; Remove SCHEDULED/DEADLINE lines (they're in metadata)
              (while (re-search-forward "^[ \t]*\\(?:SCHEDULED\\|DEADLINE\\|CLOSED\\):.*$" nil t)
                (replace-match ""))
              ;; Remove properties drawer
              (goto-char (point-min))
              (when (re-search-forward "^[ \t]*:PROPERTIES:[ \t]*$" nil t)
                (let ((drawer-start (match-beginning 0)))
                  (when (re-search-forward "^[ \t]*:END:[ \t]*$" nil t)
                    (delete-region drawer-start (point))
                    ;; Remove trailing newline
                    (when (looking-at "\n")
                      (delete-char 1)))))
              ;; Trim whitespace
              (goto-char (point-min))
              (while (re-search-forward "^[ \t]+$" nil t)
                (replace-match ""))
              (goto-char (point-min))
              (skip-chars-forward "\n")
              (delete-region (point-min) (point))
              (goto-char (point-max))
              (skip-chars-backward "\n")
              (delete-region (point) (point-max))
              (buffer-string))))
      "")))

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
          (heading-regexp (concat "^\\(\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\)\\) \\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading)))
          (priority-cookie (format " [#%d]" priority)))
      (when (re-search-forward heading-regexp nil t)
        (goto-char (match-end 1))  ; Move to end of TODO keyword
        ;; Remove existing priority if present
        (when (looking-at " \\[#[1-5]\\]")
          (delete-region (point) (+ (point) 5)))
        ;; Insert new priority (note: priority-cookie already has leading space)
        (insert priority-cookie)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-add-tags (file heading new-tags)
  "Add NEW-TAGS to task with HEADING in FILE.
NEW-TAGS is a list of tag strings to add (existing tags are preserved).
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
        (let* ((current-tags (org-get-tags))
               (combined-tags (delete-dups (append current-tags new-tags))))
          (org-set-tags combined-tags)
          (write-region (point-min) (point-max) file)
          (setq found t)))
      found)))

(defun org-batch-remove-tags (file heading tags-to-remove)
  "Remove TAGS-TO-REMOVE from task with HEADING in FILE.
TAGS-TO-REMOVE is a list of tag strings to remove.
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
        (let* ((current-tags (org-get-tags))
               (remaining-tags (seq-difference current-tags tags-to-remove)))
          (org-set-tags remaining-tags)
          (write-region (point-min) (point-max) file)
          (setq found t)))
      found)))

(defun org-batch-replace-tags (file heading new-tags)
  "Replace all tags on task with HEADING in FILE with NEW-TAGS.
NEW-TAGS is a list of tag strings.
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
        (org-set-tags new-tags)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-list-all-tags (file)
  "List all unique tags used in FILE.
Returns sorted list of tag strings."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (let ((all-tags '()))
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((tags (org-element-property :tags hl)))
            (when tags
              (setq all-tags (append all-tags tags))))))
      (sort (delete-dups all-tags) #'string<))))

(defun org-batch-get-overdue (file)
  "Get all tasks with DEADLINE before today from FILE.
Returns list of overdue tasks with their metadata."
  (let ((today (format-time-string "%Y-%m-%d"))
        (overdue-items '()))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((todo (org-element-property :todo-keyword hl))
                (deadline (org-element-property :deadline hl)))
            ;; Only include active TODOs with deadlines
            (when (and todo
                       (not (member todo '("DONE" "CANX")))
                       deadline)
              (let ((deadline-date (org-element-property :raw-value deadline)))
                ;; Extract YYYY-MM-DD from deadline
                (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" deadline-date)
                  (let ((dl-str (match-string 0 deadline-date)))
                    ;; Compare dates (string comparison works for YYYY-MM-DD)
                    (when (string< dl-str today)
                      (push (org-batch--element-to-alist hl) overdue-items)))))))))
      (nreverse overdue-items))))

(defun org-batch-get-upcoming (file &optional days)
  "Get tasks with DEADLINE or SCHEDULED in next DAYS from FILE.
DAYS defaults to 7. Returns list of upcoming tasks."
  (let* ((days-count (or days 7))
         (today (current-time))
         (future-date (time-add today (days-to-time days-count)))
         (today-str (format-time-string "%Y-%m-%d" today))
         (future-str (format-time-string "%Y-%m-%d" future-date))
         (upcoming-items '()))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (org-element-map (org-element-parse-buffer) 'headline
        (lambda (hl)
          (let ((todo (org-element-property :todo-keyword hl))
                (scheduled (org-element-property :scheduled hl))
                (deadline (org-element-property :deadline hl)))
            ;; Only include active TODOs
            (when (and todo (not (member todo '("DONE" "CANX"))))
              (let ((dates-to-check '()))
                ;; Collect scheduled and deadline dates
                (when scheduled
                  (push (org-element-property :raw-value scheduled) dates-to-check))
                (when deadline
                  (push (org-element-property :raw-value deadline) dates-to-check))
                ;; Check if any date is in range
                (dolist (date-str dates-to-check)
                  (when (string-match "\\([0-9]\\{4\\}\\)-\\([0-9]\\{2\\}\\)-\\([0-9]\\{2\\}\\)" date-str)
                    (let ((task-date (match-string 0 date-str)))
                      ;; Date is upcoming if: today <= date <= future
                      (when (and (not (string< task-date today-str))
                                 (not (string< future-str task-date)))
                        (push (org-batch--element-to-alist hl) upcoming-items)
                        ;; Stop checking other dates for this task
                        (setq dates-to-check nil))))))))))
      ;; Remove duplicates and reverse
      (delete-dups (nreverse upcoming-items)))))

(defun org-batch-get-property (file heading property-name)
  "Get value of PROPERTY-NAME for task with HEADING in FILE.
Returns property value string or nil if not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\|DONE\\|CANX\\)? ?\\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (setq found (org-entry-get nil property-name)))
      found)))

(defun org-batch-set-property (file heading property-name value)
  "Set PROPERTY-NAME to VALUE for task with HEADING in FILE.
Returns t on success, nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\|DONE\\|CANX\\)? ?\\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (org-set-property property-name value)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-list-properties (file heading)
  "List all properties for task with HEADING in FILE.
Returns alist of (property . value) pairs."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((properties '())
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\|DONE\\|CANX\\)? ?\\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        ;; Get all properties using org-entry-properties
        (let ((props (org-entry-properties nil 'standard)))
          (dolist (prop props)
            (let ((key (car prop))
                  (val (cdr prop)))
              ;; Filter out special properties we don't want to show
              (unless (member key '("CATEGORY" "BLOCKED" "ALLTAGS" "FILE" "PRIORITY_COOKIE"
                                   "TODO" "TAGS" "ITEM"))
                (push (cons key val) properties))))))
      (nreverse properties))))

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

;;; Bulk Operations

(defun org-batch-bulk-update-state (file filter-state new-state &optional filter-tags)
  "Update all tasks matching FILTER-STATE in FILE to NEW-STATE.
FILTER-TAGS: Optional list of tags to further filter tasks.
Returns count of updated tasks."
  (let ((count 0))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (org-back-to-heading t)
        (let ((todo (org-get-todo-state))
              (tags (org-get-tags)))
          (when (and todo
                     (string= todo filter-state)
                     ;; Tag filter (match any)
                     (or (null filter-tags)
                         (and tags (seq-intersection filter-tags tags))))
            (let ((org-log-done (if (string= new-state "DONE") 'time nil)))
              (org-todo new-state))
            (setq count (1+ count))))
        (forward-line 1))
      (write-region (point-min) (point-max) file))
    count))

(defun org-batch-bulk-add-tags (file filter-state new-tags)
  "Add NEW-TAGS to all tasks with FILTER-STATE in FILE.
Returns count of updated tasks."
  (let ((count 0))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (org-back-to-heading t)
        (let ((todo (org-get-todo-state)))
          (when (and todo (string= todo filter-state))
            (let* ((current-tags (org-get-tags))
                   (combined-tags (delete-dups (append current-tags new-tags))))
              (org-set-tags combined-tags))
            (setq count (1+ count))))
        (forward-line 1))
      (write-region (point-min) (point-max) file))
    count))

(defun org-batch-bulk-set-priority (file filter-state priority)
  "Set PRIORITY for all tasks with FILTER-STATE in FILE.
Returns count of updated tasks."
  (let ((count 0)
        (priority-cookie (format " [#%d]" priority)))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (goto-char (point-min))
      (while (re-search-forward (concat "^\\(\\*+ " (regexp-quote filter-state) "\\) \\(?:\\[#[1-5]\\] \\)?") nil t)
        (goto-char (match-end 1))
        ;; Remove existing priority if present
        (when (looking-at " \\[#[1-5]\\]")
          (delete-region (point) (+ (point) 5)))
        ;; Insert new priority
        (insert priority-cookie)
        (setq count (1+ count)))
      (write-region (point-min) (point-max) file))
    count))

;;; Time Tracking

(defun org-batch-clock-in (file heading)
  "Clock in to task with HEADING in FILE.
Returns t on success, nil if heading not found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil)
          (heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\|DONE\\|CANX\\)? ?\\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading))))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (org-clock-in)
        (write-region (point-min) (point-max) file)
        (setq found t))
      found)))

(defun org-batch-clock-out (file)
  "Clock out of currently clocked task in FILE.
Returns t on success, nil if no active clock found."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((found nil))
      ;; Find active clock line (has start time but no end time)
      (when (re-search-forward "^\\([ \t]*CLOCK: \\)\\(\\[.*?\\]\\)$" nil t)
        (let ((indent (match-string 1))
              (start-time (match-string 2))
              (end-time (format-time-string "[%Y-%m-%d %a %H:%M]")))
          ;; Calculate duration
          (let* ((start-ts (org-parse-time-string start-time))
                 (start-encoded (apply #'encode-time start-ts))
                 (end-encoded (current-time))
                 (duration-seconds (float-time (time-subtract end-encoded start-encoded)))
                 (hours (floor (/ duration-seconds 3600)))
                 (minutes (floor (/ (mod duration-seconds 3600) 60))))
            ;; Replace the line with closed clock entry
            (replace-match (format "%s%s--%s => %2d:%02d" indent start-time end-time hours minutes))
            (write-region (point-min) (point-max) file)
            (setq found t))))
      found)))

(defun org-batch-get-active-clock (file)
  "Get currently active clock in FILE.
Returns alist with heading and clock-in time, or nil if no active clock."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((result nil))
      ;; Find active clock line (no end time)
      (when (re-search-forward "^[ \t]*CLOCK: \\(\\[.*?\\]\\)$" nil t)
        (let ((clock-start (match-string 1)))
          (org-back-to-heading)
          (let ((heading (org-element-property :raw-value (org-element-at-point))))
            (setq result `((heading . ,heading)
                          (clock_start . ,clock-start))))))
      result)))

(defun org-batch-get-clocked-time (file heading)
  "Get total clocked time for HEADING in FILE.
Returns minutes as integer."
  (with-temp-buffer
    (insert-file-contents file)
    (org-mode)
    (goto-char (point-min))
    (let ((heading-regexp (concat "^\\*+ \\(?:TODO\\|NEXT\\|STRT\\|WAIT\\|DONE\\|CANX\\)? ?\\(?:\\[#[1-5]\\] \\)?"
                                  (regexp-quote heading)))
          (total-minutes 0))
      (when (re-search-forward heading-regexp nil t)
        (org-back-to-heading)
        (save-restriction
          (org-narrow-to-subtree)
          (org-clock-sum)
          (setq total-minutes (get-text-property (point) :org-clock-minutes))))
      (or total-minutes 0))))

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
