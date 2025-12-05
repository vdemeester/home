# Debug Workflow

Debug Emacs Lisp code using edebug, the debugger, and diagnostic tools.

## edebug (Recommended)

### Instrument Function

```elisp
;; Place cursor in function definition
(defun my-function (arg)
  "Example function."
  (let ((result (* arg 2)))
    (+ result 10)))

;; Instrument for debugging
C-u C-M-x  ; or M-x edebug-defun
```

### Debug Controls

| Key | Action |
|-----|--------|
| `SPC` | Step through (next form) |
| `n` | Next (skip over function calls) |
| `i` | Step into function |
| `o` | Step out of function |
| `g` | Go (continue execution) |
| `b` | Set breakpoint |
| `u` | Unset breakpoint |
| `e` | Evaluate expression |
| `q` | Quit debugging |
| `?` | Show help |

### Breakpoints

```elisp
(defun my-function (arg)
  (let ((x (* arg 2)))
    (edebug)  ; Hard-coded breakpoint
    (+ x 10)))

;; Or set breakpoint interactively
;; While stopped in edebug, press 'b' at any point
```

### Conditional Breakpoints

```elisp
(defun process-list (items)
  (dolist (item items)
    (when (edebug-break-when (> item 100))  ; Break if item > 100
      (process-item item))))
```

### View Variables

```elisp
;; While in edebug:
e  ; Evaluate expression
;; Type: my-var RET
;; Shows value of my-var

;; Or evaluate any expression
e (+ 1 2) RET  ; Shows 3
```

## debug (Built-in Debugger)

### Enable Debugger

```elisp
;; Debug on error
M-x toggle-debug-on-error

;; Or in code
(setq debug-on-error t)

;; Debug on quit (C-g)
(setq debug-on-quit t)
```

### Trigger Debugger

```elisp
(debug)  ; Explicit breakpoint

;; Or cause an error
(error "Debug this")
```

### Debugger Buffer

When debugger activates:

```
Debugger entered--Lisp error: (wrong-type-argument number-or-marker-p nil)
  +(nil 5)
  my-function(nil)
  eval((my-function nil) nil)
  ...
```

Controls:
- `c` - Continue
- `d` - Step through
- `e` - Evaluate expression
- `q` - Quit
- `r` - Return value
- `b` - Set breakpoint

## Diagnostic Tools

### message

```elisp
(defun my-function (arg)
  (message "arg: %S" arg)  ; Log to *Messages*
  (let ((result (* arg 2)))
    (message "result: %S" result)
    (+ result 10)))
```

### prin1 / princ

```elisp
(defun my-function (items)
  (prin1 items)  ; Print to current buffer
  (terpri)       ; Newline
  ...)
```

### trace-function

```elisp
;; Enable tracing
M-x trace-function RET my-function RET

;; Run function
(my-function 5)

;; View trace output in *trace-output*
;; Shows: arguments, return value, depth

;; Disable
M-x untrace-function RET my-function RET

;; Trace all calls
M-x trace-function-foreground RET my-function RET
```

Example trace output:
```
1 -> (my-function 5)
1 <- my-function: 20
```

### benchmark

```elisp
(require 'benchmark)

;; Time single call
(benchmark-run 1
  (my-expensive-function))
;; => (0.523 0 0.0)  ; seconds, GCs, GC time

;; Time multiple calls
(benchmark-run 1000
  (my-function 10))

;; With formatted output
(benchmark-run-compiled 1000
  (my-function 10))
```

### profiler

```elisp
;; Start CPU profiler
M-x profiler-start RET cpu RET

;; Run code to profile
(dotimes (i 10000)
  (my-function i))

;; View report
M-x profiler-report

;; Stop profiling
M-x profiler-stop
```

Profiler report shows:
- Function call counts
- Time spent in each function
- Call tree

## Common Debugging Patterns

### Check Variable Value

```elisp
(defun my-function (arg)
  (let ((result (expensive-calculation arg)))
    ;; Check value
    (unless (numberp result)
      (error "Expected number, got: %S" result))
    result))
```

### Assert

```elisp
(defun my-function (arg)
  (cl-assert (numberp arg) t "ARG must be a number, got %S" arg)
  (cl-assert (> arg 0) t "ARG must be positive, got %S" arg)
  (* arg 2))
```

### Conditional Logging

```elisp
(defvar my-package-debug nil
  "Enable debug logging when non-nil.")

(defun my-package--debug (format-string &rest args)
  "Log debug message if debugging is enabled."
  (when my-package-debug
    (apply #'message (concat "DEBUG: " format-string) args)))

(defun my-function (arg)
  (my-package--debug "Processing arg: %S" arg)
  ...)
```

### Backtrace

```elisp
(defun my-function ()
  ;; Print backtrace
  (message "Backtrace:\n%s" (backtrace-to-string))
  ...)
```

## Debugging Macros

### macroexpand

```elisp
;; Expand macro once
(macroexpand '(when condition body))
;; => (if condition (progn body))

;; Expand completely
(macroexpand-all '(when condition (when other more)))

;; Interactive
M-x pp-macroexpand-last-sexp
```

### Example Macro Debugging

```elisp
(defmacro my-with-temp-buffer (&rest body)
  `(with-temp-buffer
     (message "Entering temp buffer")
     ,@body
     (message "Exiting temp buffer")))

;; Check expansion
(macroexpand '(my-with-temp-buffer
               (insert "test")))
```

## Interactive Debugging

### eval-expression

```elisp
M-: (my-function 5) RET
;; Shows result in minibuffer
```

### ielm (Emacs Lisp REPL)

```elisp
M-x ielm

ELISP> (setq x 10)
10
ELISP> (* x 2)
20
ELISP> (require 'my-package)
t
ELISP> (my-package-test-function)
```

### scratch Buffer

```elisp
;; In *scratch* buffer
(defun test ()
  (* 2 3))

;; Evaluate
C-x C-e  ; At end of form
;; => 6

;; Evaluate and insert result
C-u C-x C-e
;; => (defun test () (* 2 3))6
```

## Debugging Strategies

### Binary Search

```elisp
(defun complex-function (arg)
  (let ((step1 (do-step-1 arg)))
    (message "After step 1: %S" step1)  ; Add checkpoints
    (let ((step2 (do-step-2 step1)))
      (message "After step 2: %S" step2)
      (let ((step3 (do-step-3 step2)))
        (message "After step 3: %S" step3)
        step3))))
```

### Simplify

```elisp
;; Original complex function
(defun complex-function (arg)
  (complicated-logic arg))

;; Simplified version for testing
(defun complex-function (arg)
  ;; (complicated-logic arg)
  42)  ; Replace with simple value

;; Gradually uncomment to find issue
```

### Isolate

```elisp
;; Extract problematic code
(defun test-isolated ()
  "Test just the problematic part."
  (let ((test-input '(1 2 3)))
    (problematic-function test-input)))

;; Run in isolation
(test-isolated)
```

## Debugging Hooks

### Trace Hook Execution

```elisp
(defun trace-hook (hook-var)
  "Trace execution of hooks in HOOK-VAR."
  (dolist (func (symbol-value hook-var))
    (trace-function func)))

;; Example
(trace-hook 'emacs-lisp-mode-hook)
```

### Debug Mode Activation

```elisp
(defun my-mode ()
  "My mode with debugging."
  (interactive)
  (message "Activating my-mode")
  (message "Hooks: %S" my-mode-hook)

  ;; Wrap hook execution
  (condition-case err
      (run-hooks 'my-mode-hook)
    (error
     (message "Error in hook: %s" (error-message-string err)))))
```

## Performance Debugging

### Time Function Calls

```elisp
(defun time-it (func &rest args)
  "Time execution of FUNC with ARGS."
  (let ((start (current-time)))
    (apply func args)
    (float-time (time-since start))))

;; Usage
(time-it #'my-expensive-function arg1 arg2)
```

### Memory Usage

```elisp
;; Before
(garbage-collect)
(let ((mem-before (memory-use-counts)))

  ;; Run code
  (my-function)

  ;; After
  (garbage-collect)
  (let ((mem-after (memory-use-counts)))
    (message "Memory used: %S"
             (cl-mapcar #'- mem-after mem-before))))
```

## Debugging Buffer Issues

### Inspect Buffer

```elisp
;; View buffer properties
(buffer-local-variables)

;; Check major mode
major-mode

;; Check buffer state
(buffer-modified-p)
(buffer-file-name)
(point)
(point-min)
(point-max)
```

### Debugging Point Movement

```elisp
(defun my-function ()
  (save-excursion  ; Save point
    (message "Point before: %d" (point))
    (goto-char (point-min))
    (message "Point during: %d" (point))
    (some-operation)
    (message "Point after: %d" (point))))
```

## Common Issues

### Wrong Type Argument

```elisp
;; Error: (wrong-type-argument number-or-marker-p nil)

;; Debug
(defun my-function (arg)
  (unless (numberp arg)
    (error "Expected number, got: %S (type: %S)"
           arg (type-of arg)))
  (+ arg 5))
```

### Void Variable

```elisp
;; Error: (void-variable my-var)

;; Check if bound
(boundp 'my-var)  ; => nil

;; Fix
(defvar my-var nil "Documentation")
```

### Void Function

```elisp
;; Error: (void-function my-func)

;; Check if defined
(fboundp 'my-func)  ; => nil

;; Check if it's a macro
(macrop 'my-func)
```

## Best Practices

1. **Use edebug**: Most powerful debugging tool
2. **Add logging**: Strategic `message` calls
3. **Write tests**: Find bugs early
4. **Check types**: Validate arguments
5. **Use assertions**: cl-assert for invariants
6. **Profile first**: Find real bottlenecks
7. **Simplify**: Break complex functions
8. **Read backtraces**: Understand error source
9. **Test incrementally**: Small changes
10. **Document assumptions**: Why code should work

## Resources

- [edebug Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Edebug.html)
- [Debugging Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Debugging.html)
- [Profiler](https://www.gnu.org/software/emacs/manual/html_node/elisp/Profiling.html)
