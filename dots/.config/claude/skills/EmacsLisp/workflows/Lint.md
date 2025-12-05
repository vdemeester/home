# Lint Workflow

Check code quality, style, and MELPA compliance using package-lint, checkdoc, and byte-compilation.

## Linting Tools

| Tool | Purpose | When to Use |
|------|---------|-------------|
| **package-lint** | MELPA compliance | Before publishing |
| **checkdoc** | Docstring quality | Always |
| **byte-compile** | Code correctness | Always |
| **elisp-lint** | Comprehensive linting | CI/CD |
| **flycheck** | Real-time linting | Development |

## package-lint (MELPA Compliance)

### Installation

```bash
# With Eask
eask install-deps --dev

# Or manually
M-x package-install RET package-lint RET
```

### Run

```elisp
;; Interactive
M-x package-lint-current-buffer

;; Batch mode
emacs -batch -l package-lint.el -f package-lint-batch-and-exit my-package.el

;; With Eask
eask lint package
```

### Common Issues

#### Missing Package Headers

```elisp
;; Bad - missing headers
;;; my-package.el --- Description

;; Good - all required headers
;;; my-package.el --- Description  -*- lexical-binding: t -*-
;; Author: Name <email>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: tools
;; URL: https://github.com/user/repo
```

#### Package-Requires Format

```elisp
;; Bad - wrong format
;; Package-Requires: (emacs "29.1")

;; Good - list of lists
;; Package-Requires: ((emacs "29.1"))

;; With dependencies
;; Package-Requires: ((emacs "29.1") (dash "2.19"))
```

#### Symbol Naming

```elisp
;; Bad - no package prefix
(defun enable-feature ()
  ...)

;; Good - consistent prefix
(defun my-package-enable-feature ()
  ...)

;; Bad - inconsistent prefix
(defun my-pkg-enable ()  ; Different from package name
  ...)

;; Good - matches package name
(defun my-package-enable ()
  ...)
```

## checkdoc (Docstring Quality)

### Run

```elisp
;; Interactive
M-x checkdoc

;; Current buffer
M-x checkdoc-current-buffer

;; Batch mode
emacs -batch -l checkdoc -f checkdoc-file my-package.el

;; With Eask
eask lint checkdoc
```

### Docstring Rules

#### Functions

```elisp
;; Bad - incomplete docstring
(defun my-package-process (file)
  "Process file."
  ...)

;; Good - complete documentation
(defun my-package-process (file &optional verbose)
  "Process FILE and return results.

If VERBOSE is non-nil, print progress messages.

FILE should be a readable file path.  Returns a list of
processed items, or nil if FILE cannot be read.

Signals an error if FILE does not exist."
  ...)
```

#### Variables

```elisp
;; Bad
(defvar my-package-timeout 30)

;; Good
(defvar my-package-timeout 30
  "Timeout in seconds for network operations.")
```

#### Customizable Variables

```elisp
;; Good
(defcustom my-package-enable-feature t
  "Whether to enable the special feature.

When non-nil, the package will automatically enable the
feature when activated."
  :type 'boolean
  :group 'my-package)
```

### Common Docstring Issues

#### Capitalization

```elisp
;; Bad - doesn't start with capital
(defun my-func ()
  "process the data."
  ...)

;; Good
(defun my-func ()
  "Process the data."
  ...)
```

#### Period

```elisp
;; Bad - no period
(defun my-func ()
  "Process the data"
  ...)

;; Good
(defun my-func ()
  "Process the data."
  ...)
```

#### Argument Documentation

```elisp
;; Bad - mentions args in lowercase
(defun my-func (arg1 arg2)
  "Process arg1 and arg2."
  ...)

;; Good - uppercase for arguments
(defun my-func (arg1 arg2)
  "Process ARG1 and ARG2."
  ...)
```

## Byte Compilation

### Compile

```bash
# Single file
emacs -batch -f batch-byte-compile my-package.el

# All .el files
emacs -batch -f batch-byte-compile *.el

# With Eask
eask compile
```

### Common Warnings

#### Undefined Functions

```elisp
;; Warning: function `some-func' is not known to be defined

;; Fix 1: Require the package
(require 'some-package)

;; Fix 2: Declare function
(declare-function some-func "some-package" (arg1 arg2))

;; Fix 3: Suppress if intentional
(with-no-warnings
  (some-func args))
```

#### Unused Variables

```elisp
;; Warning: Unused lexical variable `unused'

;; Fix: Use underscore prefix
(defun my-func (_unused arg)
  (do-something arg))
```

#### Free Variables

```elisp
;; Warning: reference to free variable `undefined-var'

;; Fix: Declare or define
(defvar undefined-var nil
  "Documentation for the variable.")
```

## elisp-lint (Comprehensive)

### Installation

```elisp
;; In Eask
(development
 (depends-on "elisp-lint"))
```

### Run

```bash
# With Eask
eask lint elisp-lint

# Direct
elisp-lint my-package.el
```

### Configure

```elisp
;; .elisp-lint.yml (if using directly)
```

elisp-lint includes:
- checkdoc
- package-lint
- byte-compile checks
- indentation
- trailing whitespace

## Flycheck (Real-time Linting)

### Setup

```elisp
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit))

;; Add checkers
(use-package flycheck-package
  :ensure t
  :after flycheck
  :config
  (flycheck-package-setup))
```

### Checkers

- `emacs-lisp` - Byte compilation
- `emacs-lisp-checkdoc` - Docstrings
- `emacs-lisp-package` - Package headers

## Eask Linting

### Eask File

```elisp
(package "my-package" "0.1.0" "Description")

(script "lint" "eask lint package checkdoc")
(script "lint:all" "eask lint package checkdoc elisp-lint")

(development
 (depends-on "package-lint")
 (depends-on "elisp-lint"))
```

### Run

```bash
# Run configured lint script
eask run lint

# Individual linters
eask lint package
eask lint checkdoc
eask lint elisp-lint

# All together
eask lint package checkdoc elisp-lint
```

## CI/CD Linting

### GitHub Actions

```yaml
name: Lint

on: [push, pull_request]

jobs:
  lint:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: jcs090218/setup-emacs@master
        with:
          version: '29.2'

      - uses: emacs-eask/setup-eask@master

      - name: Install dependencies
        run: eask install-deps --dev

      - name: Lint package
        run: eask lint package

      - name: Check documentation
        run: eask lint checkdoc

      - name: Byte compile
        run: eask compile

      - name: Comprehensive lint
        run: eask lint elisp-lint
```

## Fix Common Issues

### Indentation

```bash
# Auto-indent file
emacs -batch my-package.el \
  --eval '(indent-region (point-min) (point-max))' \
  -f save-buffer

# Or in Emacs
M-x mark-whole-buffer
M-x indent-region
```

### Trailing Whitespace

```bash
# Remove trailing whitespace
emacs -batch my-package.el \
  --eval '(delete-trailing-whitespace)' \
  -f save-buffer

# Or in Emacs
M-x delete-trailing-whitespace
```

### Auto-format

```elisp
;; Auto-remove trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Auto-indent on save
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (add-hook 'before-save-hook
                      (lambda ()
                        (when (eq major-mode 'emacs-lisp-mode)
                          (indent-region (point-min) (point-max))))
                      nil t)))
```

## Pre-commit Hook

**.git/hooks/pre-commit**:

```bash
#!/bin/bash

echo "Running lint checks..."

# Check if eask is available
if command -v eask &> /dev/null; then
    eask lint package checkdoc || exit 1
    eask compile || exit 1
else
    echo "Eask not found, skipping lint"
fi

exit 0
```

Make executable:
```bash
chmod +x .git/hooks/pre-commit
```

## Best Practices

1. **Lint before committing**: Catch issues early
2. **Fix all warnings**: Don't ignore byte-compile warnings
3. **Complete docstrings**: Document all public functions
4. **Consistent naming**: Use package prefix everywhere
5. **CI integration**: Automate linting in CI
6. **Use lexical binding**: Always `;;; -*- lexical-binding: t -*-`
7. **Check MELPA compliance**: Use package-lint
8. **Clean code**: No trailing whitespace
9. **Proper indentation**: Use Emacs auto-indent
10. **Declare dependencies**: List all Package-Requires

## Quick Checklist

Before publishing:

- [ ] `eask lint package` passes
- [ ] `eask lint checkdoc` passes
- [ ] `eask compile` completes without warnings
- [ ] All functions have docstrings
- [ ] All variables have docstrings
- [ ] Package headers complete and correct
- [ ] Lexical binding enabled
- [ ] All symbols properly prefixed
- [ ] No trailing whitespace
- [ ] Proper indentation
- [ ] Tests pass

## Resources

- [package-lint](https://github.com/purcell/package-lint)
- [checkdoc Manual](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html)
- [Flycheck](https://www.flycheck.org/)
- [elisp-lint](https://github.com/gonewest818/elisp-lint)
