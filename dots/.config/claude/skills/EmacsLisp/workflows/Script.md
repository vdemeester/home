# Script Workflow

Write standalone Emacs Lisp scripts for automation, batch processing, and command-line tools.

## Batch Mode Basics

### Simple Script

```elisp
#!/usr/bin/emacs --script
;;; process-files.el --- Process files in batch mode

(message "Script started")

(dolist (file command-line-args-left)
  (message "Processing: %s" file)
  (with-temp-buffer
    (insert-file-contents file)
    (message "Size: %d bytes" (buffer-size))))

(message "Script completed")
```

Make executable:
```bash
chmod +x process-files.el
./process-files.el file1.txt file2.txt
```

## Shebang Options

### Direct Script

```elisp
#!/usr/bin/emacs --script
;; Runs directly, no interactive mode
```

### With Options

```elisp
#!/bin/sh
":"; exec emacs --quick --script "$0" "$@" # -*-emacs-lisp-*-
;; --quick: Skip init files
;; --script: Batch mode
```

### Load Init File

```bash
#!/bin/sh
":"; exec emacs --script "$0" "$@" # -*-emacs-lisp-*-
;; Loads ~/.emacs.d/init.el
```

## Command-Line Arguments

### Access Arguments

```elisp
#!/usr/bin/emacs --script

;; Script name
(message "Script: %s" load-file-name)

;; All arguments
(message "Args: %S" command-line-args-left)

;; Process arguments
(let ((input-file (pop command-line-args-left))
      (output-file (pop command-line-args-left)))
  (unless (and input-file output-file)
    (error "Usage: %s INPUT OUTPUT" load-file-name))

  (message "Input: %s" input-file)
  (message "Output: %s" output-file))
```

### Parse Options

```elisp
#!/usr/bin/emacs --script

(defun parse-args (args)
  "Parse command-line ARGS."
  (let ((verbose nil)
        (output nil)
        (files nil))

    (while args
      (let ((arg (pop args)))
        (cond
         ((string= arg "--verbose")
          (setq verbose t))

         ((string= arg "--output")
          (setq output (pop args)))

         ((string-prefix-p "--" arg)
          (error "Unknown option: %s" arg))

         (t
          (push arg files)))))

    (list :verbose verbose
          :output output
          :files (nreverse files))))

(let ((options (parse-args command-line-args-left)))
  (message "Options: %S" options))
```

## Common Script Patterns

### File Processing

```elisp
#!/usr/bin/emacs --script
;;; convert-encoding.el --- Convert file encoding

(defun convert-file-encoding (file from to)
  "Convert FILE from FROM encoding to TO encoding."
  (let ((content (with-temp-buffer
                   (let ((coding-system-for-read from))
                     (insert-file-contents file)
                     (buffer-string)))))
    (with-temp-buffer
      (insert content)
      (let ((coding-system-for-write to))
        (write-region (point-min) (point-max) file)))))

(dolist (file command-line-args-left)
  (message "Converting %s..." file)
  (convert-file-encoding file 'utf-8 'iso-8859-1)
  (message "Done"))
```

### Directory Processing

```elisp
#!/usr/bin/emacs --script
;;; list-el-files.el --- List all .el files

(require 'find-lisp)

(defun list-el-files (directory)
  "List all .el files in DIRECTORY recursively."
  (let ((files (find-lisp-find-files directory "\\.el$")))
    (dolist (file files)
      (princ file)
      (terpri))))

(let ((dir (or (car command-line-args-left) ".")))
  (list-el-files dir))
```

### Text Transformation

```elisp
#!/usr/bin/emacs --script
;;; markdown-to-org.el --- Convert Markdown to Org

(defun markdown-to-org (md-file org-file)
  "Convert MD-FILE to ORG-FILE."
  (with-temp-buffer
    (insert-file-contents md-file)

    ;; Headers
    (goto-char (point-min))
    (while (re-search-forward "^\\(#+\\) \\(.*\\)$" nil t)
      (let ((level (length (match-string 1)))
            (title (match-string 2)))
        (replace-match (format "%s %s" (make-string level ?*) title))))

    ;; Bold
    (goto-char (point-min))
    (while (re-search-forward "\\*\\*\\([^*]+\\)\\*\\*" nil t)
      (replace-match "*\\1*"))

    ;; Write output
    (write-region (point-min) (point-max) org-file)))

(let ((input (pop command-line-args-left))
      (output (pop command-line-args-left)))
  (markdown-to-org input output)
  (message "Converted %s to %s" input output))
```

## Exit Codes

### Success/Failure

```elisp
#!/usr/bin/emacs --script

(condition-case err
    (progn
      ;; Script logic
      (when (some-error-condition)
        (error "Something went wrong"))

      ;; Success
      (kill-emacs 0))

  (error
   (message "Error: %s" (error-message-string err))
   (kill-emacs 1)))
```

### Exit with Code

```elisp
(kill-emacs 0)   ; Success
(kill-emacs 1)   ; General error
(kill-emacs 2)   ; Misuse of command
```

## Output

### Standard Output

```elisp
;; Print to stdout
(princ "Hello, World!")
(terpri)  ; Newline

;; Or use message (goes to stderr in batch mode)
(message "Processing...")

;; Format output
(princ (format "Processed %d files\n" count))
```

### Standard Error

```elisp
;; In batch mode, message goes to stderr
(message "Warning: %s" warning-text)

;; Explicit stderr
(with-current-buffer (get-buffer-create "*stderr*")
  (insert "Error message\n"))
```

## Practical Examples

### Batch Byte Compilation

```elisp
#!/usr/bin/emacs --script
;;; batch-compile.el --- Byte compile all .el files

(require 'bytecomp)

(defun batch-byte-compile-directory (directory)
  "Byte compile all .el files in DIRECTORY."
  (let ((files (directory-files directory t "\\.el$")))
    (dolist (file files)
      (unless (string-match-p "-test\\.el$" file)
        (message "Compiling %s..." file)
        (byte-compile-file file)))))

(let ((dir (or (car command-line-args-left) ".")))
  (batch-byte-compile-directory dir))
```

### Generate Autoloads

```elisp
#!/usr/bin/emacs --script
;;; generate-autoloads.el --- Generate autoloads file

(require 'autoload)

(let ((dir (or (car command-line-args-left) "."))
      (output "my-package-autoloads.el"))

  (with-current-buffer (find-file-noselect output)
    (erase-buffer)
    (insert ";;; Autoloads\n")
    (save-buffer))

  (update-directory-autoloads dir)
  (message "Generated %s" output))
```

### Run Tests

```elisp
#!/usr/bin/emacs --script
;;; run-tests.el --- Run ERT tests

(require 'ert)

;; Load package
(add-to-list 'load-path ".")
(load "my-package.el")
(load "my-package-test.el")

;; Run tests
(ert-run-tests-batch-and-exit t)
```

### Extract Documentation

```elisp
#!/usr/bin/emacs --script
;;; extract-docs.el --- Extract function documentation

(defun extract-function-docs (file)
  "Extract function documentation from FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))

    (while (re-search-forward "^(defun \\([^ ]+\\)" nil t)
      (let ((func-name (match-string 1)))
        (forward-sexp)
        (forward-line)
        (when (looking-at "[ \t]*\"\\([^\"]+\\)\"")
          (princ (format "* %s\n  %s\n\n" func-name (match-string 1))))))))

(dolist (file command-line-args-left)
  (extract-function-docs file))
```

## Debugging Scripts

### Enable Debug

```elisp
#!/usr/bin/emacs --script

(setq debug-on-error t)

;; Your code
```

### Print Variables

```elisp
(message "Variable value: %S" my-var)
(prin1 my-var)
```

### Backtrace

```elisp
(condition-case err
    (risky-operation)
  (error
   (message "Error: %s" (error-message-string err))
   (message "Backtrace: %S" (backtrace-to-string))
   (kill-emacs 1)))
```

## Advanced Techniques

### Load Dependencies

```elisp
#!/usr/bin/emacs --script

;; Add package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; Ensure package is installed
(unless (package-installed-p 'dash)
  (package-refresh-contents)
  (package-install 'dash))

(require 'dash)

;; Use dash functions
(-map (lambda (x) (* x 2)) '(1 2 3))
```

### Parallel Processing

```elisp
#!/usr/bin/emacs --script

(defun process-file-async (file callback)
  "Process FILE asynchronously, call CALLBACK when done."
  (make-process
   :name (format "process-%s" file)
   :buffer nil
   :command (list "emacs" "--batch"
                  "--eval" (format "(progn (load \"%s\") (process-file \"%s\"))"
                                   load-file-name file))
   :sentinel (lambda (proc event)
               (when (string-match-p "finished" event)
                 (funcall callback file)))))

;; Process files in parallel
(let ((files command-line-args-left)
      (count 0))
  (dolist (file files)
    (process-file-async file
                        (lambda (f)
                          (setq count (1+ count))
                          (when (= count (length files))
                            (kill-emacs 0)))))

  ;; Wait for completion
  (while t (sleep-for 0.1)))
```

## Best Practices

1. **Use `--script`**: Faster than `--batch`
2. **Handle errors**: Use `condition-case`
3. **Check arguments**: Validate input
4. **Exit codes**: Return appropriate codes
5. **Progress messages**: Use `message` for feedback
6. **Load minimal**: Only load needed libraries
7. **Test scripts**: Like any other code
8. **Document**: Add usage at top of file
9. **Make executable**: `chmod +x`
10. **Clean output**: Separate status from data

## Resources

- [Batch Mode Documentation](https://www.gnu.org/software/emacs/manual/html_node/elisp/Batch-Mode.html)
- [Command-Line Arguments](https://www.gnu.org/software/emacs/manual/html_node/elisp/Command_002dLine-Arguments.html)
- [Scripting Emacs](https://www.masteringemacs.org/article/scripting-emacs-batch-mode)
