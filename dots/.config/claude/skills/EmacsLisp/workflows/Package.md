# Package Workflow

Create well-structured Emacs Lisp packages following modern conventions and MELPA requirements.

## Package Types

| Type | Description | Example |
|------|-------------|---------|
| **Single-file** | One `.el` file | simple-mode.el |
| **Multi-file** | Multiple `.el` files | magit (magit.el, magit-*.el) |
| **With dependencies** | Requires other packages | Uses Package-Requires |

## Creating a Package

### Step 1: Initialize with Eask

```bash
# Create package directory
mkdir my-package
cd my-package

# Initialize Eask project
eask init

# This creates:
# - Eask file
# - my-package.el (with template)
# - .gitignore
```

### Step 2: Package File Structure

**my-package.el** (main file):

```elisp
;;; my-package.el --- Brief description  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name

;; Author: Your Name <your.email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/yourusername/my-package
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Longer description of what this package does.
;;
;; Usage:
;;
;;     (require 'my-package)
;;     (my-package-enable)
;;
;; Customization:
;;
;;     (setq my-package-option value)

;;; Code:

(require 'cl-lib)  ; If using cl-lib functions

(defgroup my-package nil
  "Customization group for my-package."
  :group 'tools
  :prefix "my-package-"
  :link '(url-link "https://github.com/yourusername/my-package"))

(defcustom my-package-enable-feature t
  "Whether to enable the feature."
  :type 'boolean
  :safe #'booleanp
  :group 'my-package)

;;;###autoload
(defun my-package-enable ()
  "Enable my-package."
  (interactive)
  (my-package-mode 1)
  (message "My Package enabled"))

(defvar my-package-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c m t") #'my-package-toggle)
    map)
  "Keymap for `my-package-mode'.")

;;;###autoload
(define-minor-mode my-package-mode
  "Minor mode for my-package."
  :lighter " MyPkg"
  :keymap my-package-mode-map
  :global t
  :group 'my-package
  (if my-package-mode
      (my-package--enable)
    (my-package--disable)))

(defun my-package--enable ()
  "Internal function to enable my-package."
  ;; Setup code here
  )

(defun my-package--disable ()
  "Internal function to disable my-package."
  ;; Cleanup code here
  )

(provide 'my-package)
;;; my-package.el ends here
```

### Step 3: Eask Configuration

**Eask** file:

```elisp
(package "my-package"
         "0.1.0"
         "Brief description of my package")

(website-url "https://github.com/yourusername/my-package")
(keywords "convenience" "tools")

(package-file "my-package.el")

(script "test" "echo \"Run tests..\" && eask test ert")
(script "lint" "echo \"Linting..\" && eask lint package checkdoc")
(script "ci" "eask install-deps && eask compile && eask test && eask lint")

(source "gnu")
(source "melpa")

(depends-on "emacs" "29.1")
;; Add package dependencies here
;; (depends-on "dash")

(development
 (depends-on "ert-runner")
 (depends-on "package-lint"))
```

### Step 4: Tests

**my-package-test.el**:

```elisp
;;; my-package-test.el --- Tests for my-package  -*- lexical-binding: t -*-

(require 'ert)
(require 'my-package)

(ert-deftest my-package-test-enable ()
  "Test that enabling works."
  (my-package-mode 1)
  (should my-package-mode)
  (my-package-mode -1)
  (should-not my-package-mode))

(ert-deftest my-package-test-function ()
  "Test main functionality."
  ;; Your tests here
  (should (functionp 'my-package-enable)))

(provide 'my-package-test)
;;; my-package-test.el ends here
```

### Step 5: README.md

```markdown
# My Package

Brief description of what your package does.

## Installation

### MELPA

```elisp
(use-package my-package
  :ensure t)
```

### Manual

Clone this repository and add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/my-package")
(require 'my-package)
```

## Usage

```elisp
;; Enable my-package
(my-package-enable)

;; Or use minor mode
(my-package-mode 1)
```

## Configuration

```elisp
(setq my-package-enable-feature nil)
```

## License

GPL-3.0-or-later
```

## Multi-file Package

For larger packages with multiple files:

### Structure

```
my-package/
├── my-package.el          # Main entry point
├── my-package-core.el     # Core functionality
├── my-package-ui.el       # UI components
├── my-package-utils.el    # Utilities
├── test/
│   ├── my-package-test.el
│   └── my-package-core-test.el
├── Eask
└── README.md
```

### Main File (my-package.el)

```elisp
;;; my-package.el --- Main entry point  -*- lexical-binding: t -*-

;; [Headers as before]

;;; Code:

(require 'my-package-core)
(require 'my-package-ui)
(require 'my-package-utils)

;;;###autoload
(defun my-package-setup ()
  "Set up my-package."
  (interactive)
  (my-package-core-initialize)
  (my-package-ui-setup))

(provide 'my-package)
;;; my-package.el ends here
```

### Submodule (my-package-core.el)

```elisp
;;; my-package-core.el --- Core functionality  -*- lexical-binding: t -*-

;; Copyright (C) 2025 Your Name
;; Author: Your Name <your.email@example.com>

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Core functionality for my-package.

;;; Code:

(defun my-package-core-initialize ()
  "Initialize core functionality."
  ;; Implementation
  )

(provide 'my-package-core)
;;; my-package-core.el ends here
```

## Package Headers Reference

### Required Headers

```elisp
;; Author: Your Name <email@example.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience tools
;; URL: https://github.com/user/repo
```

### Optional Headers

```elisp
;; Maintainer: Different Person <maintainer@example.com>
;; Created: 2025-01-01
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Homepage: https://mypackage.example.com
```

### Keywords

Standard keywords (for `M-x finder-by-keyword`):

- `abbrev` - Abbreviation handling
- `calendar` - Calendar and diary
- `comm` - Communications
- `convenience` - Convenience features
- `data` - Data manipulation
- `docs` - Documentation
- `emulations` - Emulations of other editors
- `extensions` - Emacs Lisp language extensions
- `faces` - Fonts and faces
- `files` - File handling
- `frames` - Frame manipulation
- `games` - Games
- `hardware` - Hardware support
- `help` - Help and documentation
- `hypermedia` - Hypermedia
- `i18n` - Internationalization
- `internal` - Internal use
- `languages` - Programming languages
- `lisp` - Lisp support
- `local` - Local customization
- `maint` - Maintenance tools
- `mail` - Mail handling
- `matching` - Pattern matching
- `mouse` - Mouse support
- `multimedia` - Multimedia
- `news` - News handling
- `outlines` - Outline mode
- `processes` - Process control
- `terminals` - Terminal emulation
- `tex` - TeX and friends
- `tools` - Programming tools
- `unix` - Unix features
- `vc` - Version control
- `wp` - Word processing

## Common Patterns

### Customization Group

```elisp
(defgroup my-package nil
  "Customization for my-package."
  :group 'tools
  :prefix "my-package-"
  :link '(url-link :tag "GitHub" "https://github.com/user/my-package"))

(defcustom my-package-directory "~/.my-package"
  "Directory for my-package data."
  :type 'directory
  :group 'my-package)

(defcustom my-package-backends '(backend1 backend2)
  "List of backends to use."
  :type '(repeat (choice (const :tag "Backend 1" backend1)
                         (const :tag "Backend 2" backend2)))
  :group 'my-package)
```

### Faces

```elisp
(defface my-package-highlight
  '((t :inherit highlight))
  "Face for highlighted items."
  :group 'my-package)

(defface my-package-error
  '((t :inherit error))
  "Face for errors."
  :group 'my-package)
```

### Hooks

```elisp
(defvar my-package-mode-hook nil
  "Hook run after `my-package-mode' is enabled.")

(defvar my-package-before-process-hook nil
  "Hook run before processing.")

;; Run hooks
(run-hooks 'my-package-mode-hook)
```

### Autoloads

```elisp
;;;###autoload
(defun my-package-enable ()
  "Enable my-package globally."
  (interactive)
  (my-package-mode 1))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.mypkg\\'" . my-package-mode))
```

## Development Workflow

### 1. Install Dependencies

```bash
eask install-deps
```

### 2. Byte Compile

```bash
eask compile
```

### 3. Run Tests

```bash
eask test ert
```

### 4. Lint

```bash
eask lint package
eask lint checkdoc
```

### 5. Package

```bash
eask package
```

Output: `dist/my-package-0.1.0.tar`

## CI/CD with GitHub Actions

**.github/workflows/test.yml**:

```yaml
name: CI

on:
  push:
    branches: [main]
  pull_request:
    branches: [main]

jobs:
  test:
    runs-on: ${{ matrix.os }}
    strategy:
      matrix:
        os: [ubuntu-latest, macos-latest, windows-latest]
        emacs-version: ['29.1', '29.2', 'snapshot']

    steps:
      - uses: actions/checkout@v4

      - uses: jcs090218/setup-emacs@master
        with:
          version: ${{ matrix.emacs-version }}

      - uses: emacs-eask/setup-eask@master
        with:
          version: 'snapshot'

      - name: Install dependencies
        run: eask install-deps

      - name: Byte compile
        run: eask compile

      - name: Run tests
        run: eask test ert

      - name: Lint
        run: |
          eask lint package
          eask lint checkdoc
```

## Versioning

Follow [Semantic Versioning](https://semver.org/):

- **MAJOR**: Incompatible API changes
- **MINOR**: Add functionality (backwards-compatible)
- **PATCH**: Bug fixes (backwards-compatible)

Example: `1.2.3` → MAJOR.MINOR.PATCH

### Update Version

```bash
# In my-package.el header
;; Version: 1.0.0

# In Eask
(package "my-package" "1.0.0" "Description")

# Create git tag
git tag -a v1.0.0 -m "Release version 1.0.0"
git push origin v1.0.0
```

## Common Issues

### Package-Requires Format

```elisp
;; Correct
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))

;; Wrong - no spaces
;; Package-Requires: ((emacs "29.1")(dash "2.19.1"))

;; Wrong - single dep not in list
;; Package-Requires: (emacs "29.1")
```

### Lexical Binding

Always use:

```elisp
;;; my-package.el --- Description  -*- lexical-binding: t -*-
```

### Provide Statement

Must match filename:

```elisp
;; File: my-package.el
(provide 'my-package)  ; Correct

;; File: my-package.el
(provide 'my-pkg)  ; Wrong!
```

## Best Practices

1. **Use lexical binding**: `;;; -*- lexical-binding: t -*-`
2. **Follow naming conventions**: All symbols prefixed with package name
3. **Complete headers**: All required headers present
4. **Autoloads**: Mark entry points with `;;;###autoload`
5. **Tests**: Write tests for all functionality
6. **Documentation**: Complete docstrings for all public functions
7. **Version control**: Use git, tag releases
8. **CI**: Set up automated testing
9. **CHANGELOG**: Maintain version history
10. **License**: Include GPL-compatible license

## Resources

- [Emacs Package Development](https://www.gnu.org/software/emacs/manual/html_node/elisp/Packaging.html)
- [MELPA Contributing Guide](https://github.com/melpa/melpa/blob/master/CONTRIBUTING.org)
- [Eask Documentation](https://emacs-eask.github.io/)
- [Package Lint](https://github.com/purcell/package-lint)
