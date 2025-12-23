;;; batch-functions-test.el --- Tests for org-mode batch operations -*- lexical-binding: t; no-byte-compile: t -*-

;; Copyright (C) 2025 Vincent Demeester

;; Author: Vincent Demeester <vincent@sbr.pm>
;; Keywords: org, batch, automation, testing

;;; Commentary:

;; ERT tests for batch-functions.el
;; Run with: emacs --batch -L .. -l batch-functions.el -l batch-functions-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)
(require 'org)
(require 'org-element)

;; Load batch-functions from parent directory
(eval-and-compile
  (let* ((current-file (or load-file-name buffer-file-name))
         (parent-dir (when current-file
                       (file-name-directory (directory-file-name (file-name-directory current-file))))))
    (when parent-dir
      (add-to-list 'load-path parent-dir)))
  (require 'batch-functions))

;;; Test Fixtures

(defvar batch-test-fixture-dir
  (expand-file-name "fixtures" (file-name-directory load-file-name))
  "Directory containing test fixture files.")

(defvar batch-test-fixture-file
  (expand-file-name "test-todos.org" batch-test-fixture-dir)
  "Main test fixture file.")

;;; Helper Functions

(defun batch-test--count-items (items)
  "Count number of items in ITEMS list."
  (length items))

(defun batch-test--find-item-by-heading (items heading)
  "Find item in ITEMS with matching HEADING."
  (seq-find (lambda (item)
              (string= (alist-get 'heading item) heading))
            items))

(defun batch-test--with-temp-org-file (content fn)
  "Create temp org file with CONTENT, call FN with filepath, then cleanup."
  (let ((temp-file (make-temp-file "org-test-" nil ".org")))
    (unwind-protect
        (progn
          (with-temp-file temp-file
            (insert content))
          (funcall fn temp-file))
      (when (file-exists-p temp-file)
        (delete-file temp-file)))))

;;; Tests for Read Operations

(ert-deftest test-org-batch-list-todos ()
  "Test listing all TODOs from file."
  (let ((todos (org-batch-list-todos batch-test-fixture-file)))
    (should (> (batch-test--count-items todos) 0))
    ;; Should find "Review PR #123"
    (should (batch-test--find-item-by-heading todos "Review PR #123"))
    ;; Should find "Buy groceries"
    (should (batch-test--find-item-by-heading todos "Buy groceries"))))

(ert-deftest test-org-batch-list-todos-filter-state ()
  "Test filtering TODOs by state."
  (let ((next-todos (org-batch-list-todos batch-test-fixture-file "NEXT")))
    (should (> (batch-test--count-items next-todos) 0))
    ;; All items should have NEXT state
    (dolist (todo next-todos)
      (should (string= (alist-get 'todo todo) "NEXT")))
    ;; Should find "Implement authentication"
    (should (batch-test--find-item-by-heading next-todos "Implement authentication"))))

(ert-deftest test-org-batch-list-todos-filter-priority ()
  "Test filtering TODOs by priority."
  (let ((high-priority (org-batch-list-todos batch-test-fixture-file nil 1)))
    (should (> (batch-test--count-items high-priority) 0))
    ;; All items should have priority 1
    (dolist (todo high-priority)
      (should (= (alist-get 'priority todo) 1)))
    ;; Should find "Buy groceries"
    (should (batch-test--find-item-by-heading high-priority "Buy groceries"))))

(ert-deftest test-org-batch-list-todos-filter-tags ()
  "Test filtering TODOs by tags."
  (let ((work-todos (org-batch-list-todos batch-test-fixture-file nil nil '("work"))))
    (should (> (batch-test--count-items work-todos) 0))
    ;; All items should have :work: tag
    (dolist (todo work-todos)
      (should (member "work" (alist-get 'tags todo))))
    ;; Should find "Review PR #123"
    (should (batch-test--find-item-by-heading work-todos "Review PR #123"))))

(ert-deftest test-org-batch-scheduled-today ()
  "Test getting scheduled items for a specific date."
  (let ((scheduled (org-batch-scheduled-today batch-test-fixture-file "2025-12-23")))
    (should (>= (batch-test--count-items scheduled) 1))
    ;; Should include "Review PR #123" and "Buy groceries"
    (should (batch-test--find-item-by-heading scheduled "Review PR #123"))))

(ert-deftest test-org-batch-by-section ()
  "Test getting TODOs by section."
  (let ((work-section (org-batch-by-section batch-test-fixture-file "Work")))
    (should (> (batch-test--count-items work-section) 0))
    ;; Should find work-related tasks
    (should (batch-test--find-item-by-heading work-section "Review PR #123")))

  (let ((personal-section (org-batch-by-section batch-test-fixture-file "Personal")))
    (should (> (batch-test--count-items personal-section) 0))
    ;; Should find personal tasks
    (should (batch-test--find-item-by-heading personal-section "Buy groceries"))))

(ert-deftest test-org-batch-count-by-state ()
  "Test counting TODOs by state."
  (let ((counts (org-batch-count-by-state batch-test-fixture-file)))
    (should (> (alist-get 'total counts) 0))
    (should (> (alist-get 'TODO counts) 0))
    (should (>= (alist-get 'NEXT counts) 1))
    (should (>= (alist-get 'DONE counts) 1))
    (should (>= (alist-get 'WAIT counts) 1))))

(ert-deftest test-org-batch-search ()
  "Test searching for content in TODOs."
  (let ((matches (org-batch-search batch-test-fixture-file "OAuth2")))
    (should (> (batch-test--count-items matches) 0))
    ;; Should find "Implement authentication"
    (should (batch-test--find-item-by-heading matches "Implement authentication"))))

(ert-deftest test-org-batch-get-sections ()
  "Test getting list of top-level sections."
  (let ((sections (org-batch-get-sections batch-test-fixture-file)))
    (should (>= (length sections) 3))
    (should (member "Work" sections))
    (should (member "Personal" sections))
    (should (member "Archive" sections))))

(ert-deftest test-org-batch-get-children ()
  "Test getting direct children of a heading."
  (let ((work-children (org-batch-get-children batch-test-fixture-file "Work")))
    (should (>= (batch-test--count-items work-children) 2))
    ;; Should find direct children only
    (should (batch-test--find-item-by-heading work-children "Review PR #123"))
    (should (batch-test--find-item-by-heading work-children "Implement authentication"))))

;;; Tests for Write Operations

(ert-deftest test-org-batch-add-todo ()
  "Test adding a new TODO item."
  (batch-test--with-temp-org-file
   "* Work\n"
   (lambda (temp-file)
     (let ((result (org-batch-add-todo temp-file "Work" "New Task"
                                       "2025-12-25" 2 '("test" "task"))))
       (should result)
       ;; Verify it was added
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((new-task (batch-test--find-item-by-heading todos "New Task")))
           (should new-task)
           (should (string= (alist-get 'todo new-task) "TODO"))
           (should (= (alist-get 'priority new-task) 2))
           (should (member "test" (alist-get 'tags new-task)))
           (should (member "task" (alist-get 'tags new-task)))))))))

(ert-deftest test-org-batch-update-state ()
  "Test updating TODO state."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n"
   (lambda (temp-file)
     (let ((result (org-batch-update-state temp-file "Test Task" "DONE")))
       (should result)
       ;; Verify state was updated
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task (batch-test--find-item-by-heading todos "Test Task")))
           (should task)
           (should (string= (alist-get 'todo task) "DONE"))))))))

(ert-deftest test-org-batch-schedule-task ()
  "Test scheduling a task."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n"
   (lambda (temp-file)
     (let ((result (org-batch-schedule-task temp-file "Test Task" "2025-12-25")))
       (should result)
       ;; Verify schedule was set
       (let ((scheduled (org-batch-scheduled-today temp-file "2025-12-25")))
         (should (batch-test--find-item-by-heading scheduled "Test Task")))))))

(ert-deftest test-org-batch-set-priority ()
  "Test setting task priority."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n"
   (lambda (temp-file)
     (let ((result (org-batch-set-priority temp-file "Test Task" 1)))
       (should result)
       ;; Verify priority was set
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task (batch-test--find-item-by-heading todos "Test Task")))
           (should task)
           (should (= (alist-get 'priority task) 1))))))))

;;; Tests for Utility Functions

(ert-deftest test-org-batch--priority-conversion ()
  "Test priority number/character conversion.
Priority mapping: '1'=1, '2'=2, '3'=3, '4'=4, '5'=5."
  (should (= (org-batch--priority-to-number ?1) 1))
  (should (= (org-batch--priority-to-number ?5) 5))
  (should (= (org-batch--number-to-priority 1) ?1))
  (should (= (org-batch--number-to-priority 5) ?5)))

;;; Placeholder Tests for New Features

(ert-deftest test-org-batch-get-todo-content ()
  "Test getting full TODO content with metadata and body."
  (let ((result (org-batch-get-todo-content batch-test-fixture-file "Review PR #123")))
    (should result)
    ;; Check basic metadata
    (should (string= (alist-get 'heading result) "Review PR #123"))
    (should (string= (alist-get 'todo result) "TODO"))
    (should (member "work" (alist-get 'tags result)))
    (should (member "code" (alist-get 'tags result)))
    ;; Check properties
    (let ((props (alist-get 'properties result)))
      (should props)
      (should (assoc "CREATED" props))
      (should (assoc "PR_URL" props))
      (should (string-match "github.com" (cdr (assoc "PR_URL" props)))))
    ;; Check content
    (let ((content (alist-get 'content result)))
      (should content)
      (should (string-match "Check for security" content))
      (should (string-match "error handling" content))))

  ;; Test non-existent heading
  (let ((result (org-batch-get-todo-content batch-test-fixture-file "NonExistent Task")))
    (should-not result)))

(ert-deftest test-org-batch-add-tags ()
  "Test adding tags to existing TODO."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task :existing:\n"
   (lambda (temp-file)
     (let ((result (org-batch-add-tags temp-file "Test Task" '("new" "tags"))))
       (should result)
       ;; Verify tags were added
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task (batch-test--find-item-by-heading todos "Test Task")))
           (should task)
           (should (member "existing" (alist-get 'tags task)))
           (should (member "new" (alist-get 'tags task)))
           (should (member "tags" (alist-get 'tags task)))))))))

(ert-deftest test-org-batch-remove-tags ()
  "Test removing tags from TODO."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task :tag1:tag2:tag3:\n"
   (lambda (temp-file)
     (let ((result (org-batch-remove-tags temp-file "Test Task" '("tag2"))))
       (should result)
       ;; Verify tag was removed
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task (batch-test--find-item-by-heading todos "Test Task")))
           (should task)
           (should (member "tag1" (alist-get 'tags task)))
           (should-not (member "tag2" (alist-get 'tags task)))
           (should (member "tag3" (alist-get 'tags task)))))))))

(ert-deftest test-org-batch-list-all-tags ()
  "Test listing all unique tags."
  (let ((tags (org-batch-list-all-tags batch-test-fixture-file)))
    (should (> (length tags) 0))
    (should (member "work" tags))
    (should (member "code" tags))
    (should (member "personal" tags))
    ;; Tags should be sorted and unique
    (should (equal tags (sort (delete-dups tags) #'string<)))))

(ert-deftest test-org-batch-get-overdue ()
  "Test getting overdue tasks."
  ;; Create a file with overdue task
  (batch-test--with-temp-org-file
   "* Work\n** TODO Overdue Task\nDEADLINE: <2020-01-01>\n"
   (lambda (temp-file)
     (let ((overdue (org-batch-get-overdue temp-file)))
       (should (> (batch-test--count-items overdue) 0))
       (should (batch-test--find-item-by-heading overdue "Overdue Task"))))))

(ert-deftest test-org-batch-get-upcoming ()
  "Test getting upcoming tasks."
  ;; Create a file with upcoming task
  (let ((future-date (format-time-string "%Y-%m-%d" (time-add (current-time) (days-to-time 3)))))
    (batch-test--with-temp-org-file
     (format "* Work\n** TODO Upcoming Task\nSCHEDULED: <%s>\n" future-date)
     (lambda (temp-file)
       (let ((upcoming (org-batch-get-upcoming temp-file 7)))
         (should (> (batch-test--count-items upcoming) 0))
         (should (batch-test--find-item-by-heading upcoming "Upcoming Task")))))))

(ert-deftest test-org-batch-get-property ()
  "Test getting property value."
  (let ((prop-value (org-batch-get-property batch-test-fixture-file "Review PR #123" "PR_URL")))
    (should prop-value)
    (should (string-match "github.com" prop-value))))

(ert-deftest test-org-batch-set-property ()
  "Test setting property value."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n:PROPERTIES:\n:CREATED: [2025-12-23]\n:END:\n"
   (lambda (temp-file)
     (let ((result (org-batch-set-property temp-file "Test Task" "CUSTOM_PROP" "test-value")))
       (should result)
       ;; Verify property was set
       (let ((value (org-batch-get-property temp-file "Test Task" "CUSTOM_PROP")))
         (should value)
         (should (string= value "test-value")))))))

;;; Tests for Bulk Operations

(ert-deftest test-org-batch-bulk-update-state ()
  "Test bulk updating state of multiple tasks."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Task 1\n** TODO Task 2 :urgent:\n** NEXT Task 3\n"
   (lambda (temp-file)
     (let ((count (org-batch-bulk-update-state temp-file "TODO" "DONE")))
       (should (= count 2))
       ;; Verify states were updated
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task1 (batch-test--find-item-by-heading todos "Task 1"))
               (task2 (batch-test--find-item-by-heading todos "Task 2")))
           (should (string= (alist-get 'todo task1) "DONE"))
           (should (string= (alist-get 'todo task2) "DONE"))))))))

(ert-deftest test-org-batch-bulk-update-state-with-tag-filter ()
  "Test bulk updating state with tag filter."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Task 1 :urgent:\n** TODO Task 2\n** TODO Task 3 :urgent:\n"
   (lambda (temp-file)
     (let ((count (org-batch-bulk-update-state temp-file "TODO" "NEXT" '("urgent"))))
       (should (= count 2))
       ;; Verify only urgent tasks were updated
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task1 (batch-test--find-item-by-heading todos "Task 1"))
               (task2 (batch-test--find-item-by-heading todos "Task 2"))
               (task3 (batch-test--find-item-by-heading todos "Task 3")))
           (should (string= (alist-get 'todo task1) "NEXT"))
           (should (string= (alist-get 'todo task2) "TODO"))
           (should (string= (alist-get 'todo task3) "NEXT"))))))))

(ert-deftest test-org-batch-bulk-add-tags ()
  "Test bulk adding tags to multiple tasks."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Task 1\n** TODO Task 2\n** NEXT Task 3\n"
   (lambda (temp-file)
     (let ((count (org-batch-bulk-add-tags temp-file "TODO" '("review" "urgent"))))
       (should (= count 2))
       ;; Verify tags were added
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task1 (batch-test--find-item-by-heading todos "Task 1"))
               (task2 (batch-test--find-item-by-heading todos "Task 2")))
           (should (member "review" (alist-get 'tags task1)))
           (should (member "urgent" (alist-get 'tags task1)))
           (should (member "review" (alist-get 'tags task2)))
           (should (member "urgent" (alist-get 'tags task2)))))))))

(ert-deftest test-org-batch-bulk-set-priority ()
  "Test bulk setting priority for multiple tasks."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Task 1\n** TODO Task 2\n** NEXT Task 3\n"
   (lambda (temp-file)
     (let ((count (org-batch-bulk-set-priority temp-file "TODO" 1)))
       (should (= count 2))
       ;; Verify priorities were set
       (let ((todos (org-batch-list-todos temp-file)))
         (let ((task1 (batch-test--find-item-by-heading todos "Task 1"))
               (task2 (batch-test--find-item-by-heading todos "Task 2"))
               (task3 (batch-test--find-item-by-heading todos "Task 3")))
           (should (= (alist-get 'priority task1) 1))
           (should (= (alist-get 'priority task2) 1))
           (should-not (alist-get 'priority task3))))))))

;;; Tests for Time Tracking

(ert-deftest test-org-batch-clock-in ()
  "Test clocking in to a task."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n"
   (lambda (temp-file)
     (let ((result (org-batch-clock-in temp-file "Test Task")))
       (should result)
       ;; Verify clock entry was added
       (with-temp-buffer
         (insert-file-contents temp-file)
         (should (string-match-p "CLOCK: \\[" (buffer-string))))))))

(ert-deftest test-org-batch-clock-out ()
  "Test clocking out of a task."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n:LOGBOOK:\nCLOCK: [2025-12-23 Mon 10:00]\n:END:\n"
   (lambda (temp-file)
     (let ((result (org-batch-clock-out temp-file)))
       (should result)
       ;; Verify clock was closed with end time
       (with-temp-buffer
         (insert-file-contents temp-file)
         (should (string-match-p "CLOCK: \\[.*?\\]--\\[.*?\\] =>" (buffer-string))))))))

(ert-deftest test-org-batch-get-active-clock ()
  "Test getting active clock."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n:LOGBOOK:\nCLOCK: [2025-12-23 Mon 10:00]\n:END:\n"
   (lambda (temp-file)
     (let ((result (org-batch-get-active-clock temp-file)))
       (should result)
       (should (string= (alist-get 'heading result) "Test Task"))
       (should (alist-get 'clock_start result))))))

(ert-deftest test-org-batch-get-active-clock-none ()
  "Test getting active clock when none exists."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n"
   (lambda (temp-file)
     (let ((result (org-batch-get-active-clock temp-file)))
       (should-not result)))))

(ert-deftest test-org-batch-get-clocked-time ()
  "Test getting total clocked time for a task."
  (batch-test--with-temp-org-file
   "* Work\n** TODO Test Task\n:LOGBOOK:\nCLOCK: [2025-12-23 Mon 10:00]--[2025-12-23 Mon 11:30] =>  1:30\n:END:\n"
   (lambda (temp-file)
     (let ((minutes (org-batch-get-clocked-time temp-file "Test Task")))
       (should (> minutes 0))
       (should (= minutes 90))))))  ; 1:30 = 90 minutes

(provide 'batch-functions-test)
;;; batch-functions-test.el ends here
