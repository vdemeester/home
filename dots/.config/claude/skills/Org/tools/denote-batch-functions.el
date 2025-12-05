;;; denote-batch-functions.el --- Batch operations for denote notes -*- lexical-binding: t; no-byte-compile: t; -*-

;; Copyright (C) 2025 Vincent Demeester

;; This file provides batch mode functions for creating and manipulating
;; denote-formatted notes from the command line using the denote package.
;;
;; NOTE: This file requires the denote package to be installed in your Emacs
;; configuration. It cannot be byte-compiled in isolation.

;;; Commentary:

;; These functions enable Claude Code and other tools to create denote notes
;; programmatically using Emacs batch mode. They wrap the denote package's
;; functions for non-interactive use.
;;
;; Usage:
;;   emacs --batch -l denote-batch-functions.el \
;;     --eval "(denote-batch-create-note \"Title\" '(tag1 tag2))"

;;; Code:

(require 'denote)
(require 'json)

;; Ensure denote-directory is set
(unless (boundp 'denote-directory)
  (setq denote-directory "~/desktop/org/notes/"))

;; Helper to output JSON
(defun denote-batch--output-json (data)
  "Output DATA as JSON to stdout."
  (princ (json-encode data))
  (princ "\n"))

;; Main function: Create denote note using denote package
(defun denote-batch-create-note (title keywords &optional signature category directory)
  "Create a denote note with TITLE and KEYWORDS using denote package.
KEYWORDS can be a list of strings or symbols (will be converted to strings).
Optional SIGNATURE for automated notes (e.g., \"pkai\").
Optional CATEGORY is stored in frontmatter.
Optional DIRECTORY (defaults to denote-directory).

Returns JSON with created file path."
  (condition-case err
      (let* ((denote-directory (or directory denote-directory))
             ;; Convert keywords to strings if they're symbols
             (keywords-list (mapcar (lambda (k)
                                      (if (symbolp k)
                                          (symbol-name k)
                                        k))
                                    keywords))
             ;; Use denote to create the note
             (filepath (denote title keywords-list 'org denote-directory nil nil signature nil)))

        ;; Add category to frontmatter if provided
        (when (and filepath category)
          (with-current-buffer (find-file-noselect filepath)
            (goto-char (point-min))
            ;; Find end of frontmatter
            (when (re-search-forward "^#\\+identifier:" nil t)
              (end-of-line)
              (insert (format "\n#+category: %s" category)))
            (save-buffer)
            (kill-buffer)))

        ;; Return JSON result
        (denote-batch--output-json
         (list :success t
               :filepath filepath
               :message (format "Created note: %s" (file-name-nondirectory filepath)))))
    (error
     (denote-batch--output-json
      (list :success :json-false
            :error (error-message-string err))))))

;; Create note with content from file
(defun denote-batch-create-note-from-file (title keywords content-file &optional signature category directory)
  "Create denote note with TITLE and KEYWORDS, reading content from CONTENT-FILE.
KEYWORDS can be a list of strings or symbols (will be converted to strings).
Uses denote package for creation, then appends content from file.
Optional SIGNATURE, CATEGORY, DIRECTORY same as denote-batch-create-note."
  (condition-case err
      (let* ((denote-directory (or directory denote-directory))
             ;; Convert keywords to strings if they're symbols
             (keywords-list (mapcar (lambda (k)
                                      (if (symbolp k)
                                          (symbol-name k)
                                        k))
                                    keywords))
             ;; Create the note using denote
             (filepath (denote title keywords-list 'org denote-directory nil nil signature nil)))

        ;; Add category if provided
        (when category
          (with-current-buffer (find-file-noselect filepath)
            (goto-char (point-min))
            (when (re-search-forward "^#\\+identifier:" nil t)
              (end-of-line)
              (insert (format "\n#+category: %s" category)))
            (save-buffer)
            (kill-buffer)))

        ;; Append content from file
        (when (file-exists-p content-file)
          (with-current-buffer (find-file-noselect filepath)
            (goto-char (point-max))
            (insert-file-contents content-file)
            (save-buffer)
            (kill-buffer)))

        ;; Return JSON result
        (denote-batch--output-json
         (list :success t
               :filepath filepath
               :message (format "Created note: %s" (file-name-nondirectory filepath)))))
    (error
     (denote-batch--output-json
      (list :success :json-false
            :error (error-message-string err))))))

;; Add content to existing denote note
(defun denote-batch-append-content (filepath content)
  "Append CONTENT to existing denote note at FILEPATH."
  (condition-case err
      (progn
        (unless (file-exists-p filepath)
          (error "File does not exist: %s" filepath))
        (with-current-buffer (find-file-noselect filepath)
          (goto-char (point-max))
          ;; Ensure we're on a new line
          (unless (bolp)
            (insert "\n"))
          (insert "\n" content "\n")
          (save-buffer)
          (kill-buffer))
        (denote-batch--output-json
         (list :success t
               :filepath filepath
               :message "Content appended")))
    (error
     (denote-batch--output-json
      (list :success :json-false
            :error (error-message-string err))))))

;; Update denote note using denote-rename functions
(defun denote-batch-update-frontmatter (filepath &optional new-title new-keywords new-category)
  "Update frontmatter of denote note at FILEPATH.
Optional NEW-TITLE to change title.
Optional NEW-KEYWORDS (list of symbols) to change keywords.
Optional NEW-CATEGORY to update category.

Uses denote-rename-file-using-front-matter when possible."
  (condition-case err
      (progn
        (unless (file-exists-p filepath)
          (error "File does not exist: %s" filepath))

        (with-current-buffer (find-file-noselect filepath)
          ;; Update title in frontmatter
          (when new-title
            (goto-char (point-min))
            (when (re-search-forward "^#\\+title:[ \t]*\\(.*\\)$" nil t)
              (replace-match new-title nil nil nil 1)))

          ;; Update keywords in frontmatter
          (when new-keywords
            (goto-char (point-min))
            (when (re-search-forward "^#\\+filetags:[ \t]*\\(.*\\)$" nil t)
              (let ((tags-string (concat ":" (mapconcat #'symbol-name new-keywords ":") ":")))
                (replace-match tags-string nil nil nil 1))))

          ;; Update category
          (when new-category
            (goto-char (point-min))
            (if (re-search-forward "^#\\+category:[ \t]*\\(.*\\)$" nil t)
                (replace-match new-category nil nil nil 1)
              ;; Add category if it doesn't exist
              (when (re-search-forward "^#\\+identifier:" nil t)
                (end-of-line)
                (insert (format "\n#+category: %s" new-category)))))

          (save-buffer)

          ;; Use denote-rename-file-using-front-matter to update filename
          (when (or new-title new-keywords)
            (denote-rename-file-using-front-matter filepath))

          (kill-buffer))

        (denote-batch--output-json
         (list :success t
               :filepath filepath
               :message "Frontmatter updated")))
    (error
     (denote-batch--output-json
      (list :success :json-false
            :error (error-message-string err))))))

;; Read denote note metadata using denote functions
(defun denote-batch-read-metadata (filepath)
  "Read metadata from denote note at FILEPATH using denote functions.
Returns JSON with title, keywords, identifier, signature, date, and category."
  (condition-case err
      (progn
        (unless (file-exists-p filepath)
          (error "File does not exist: %s" filepath))

        ;; Use denote's built-in metadata retrieval
        (let* ((file-type (denote-filetype-heuristics filepath))
               (title (denote-retrieve-title-value filepath file-type))
               (keywords (denote-extract-keywords-from-path filepath))
               (identifier (denote-retrieve-filename-identifier filepath))
               (signature (denote-retrieve-filename-signature filepath))
               (date-string nil)
               (category nil))

          ;; Get date and category from frontmatter
          (with-temp-buffer
            (insert-file-contents filepath)
            (goto-char (point-min))
            (when (re-search-forward "^#\\+date:[ \t]*\\(.*\\)$" nil t)
              (setq date-string (match-string 1)))
            (goto-char (point-min))
            (when (re-search-forward "^#\\+category:[ \t]*\\(.*\\)$" nil t)
              (setq category (match-string 1))))

          ;; Return JSON
          (denote-batch--output-json
           (list :success t
                 :title title
                 :keywords keywords
                 :identifier identifier
                 :signature (or signature "")
                 :date date-string
                 :category (or category "")
                 :filepath filepath))))
    (error
     (denote-batch--output-json
      (list :success :json-false
            :error (error-message-string err))))))

(provide 'denote-batch-functions)

;;; denote-batch-functions.el ends here
