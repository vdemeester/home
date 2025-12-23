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
  "Test priority number/character conversion."
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
  "Test adding tags to existing TODO (to be implemented)."
  :expected-result :failed
  (should nil))

(ert-deftest test-org-batch-remove-tags ()
  "Test removing tags from TODO (to be implemented)."
  :expected-result :failed
  (should nil))

(ert-deftest test-org-batch-list-all-tags ()
  "Test listing all unique tags (to be implemented)."
  :expected-result :failed
  (should nil))

(ert-deftest test-org-batch-get-overdue ()
  "Test getting overdue tasks (to be implemented)."
  :expected-result :failed
  (should nil))

(ert-deftest test-org-batch-get-upcoming ()
  "Test getting upcoming tasks (to be implemented)."
  :expected-result :failed
  (should nil))

(ert-deftest test-org-batch-get-property ()
  "Test getting property value (to be implemented)."
  :expected-result :failed
  (should nil))

(ert-deftest test-org-batch-set-property ()
  "Test setting property value (to be implemented)."
  :expected-result :failed
  (should nil))

(provide 'batch-functions-test)
;;; batch-functions-test.el ends here
