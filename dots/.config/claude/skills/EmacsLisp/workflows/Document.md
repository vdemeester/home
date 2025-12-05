# Document Workflow

Write comprehensive documentation for Emacs Lisp packages: docstrings, README, and Info manuals.

## Docstrings

### Function Docstrings

```elisp
(defun my-package-process-file (file &optional verbose callback)
  "Process FILE and return the result.

FILE should be a path to a readable file.  Optional argument
VERBOSE, when non-nil, enables progress messages.  Optional
argument CALLBACK is called with the result when processing
completes.

The function reads FILE, processes its contents, and returns
a list of processed items.  If FILE cannot be read, returns nil.

Example usage:

  (my-package-process-file \"input.txt\" t)
  (my-package-process-file \"data.csv\" nil #'my-callback)

Returns a list of the form (ITEM1 ITEM2 ...), or nil if FILE
cannot be processed.

Signals an error if FILE does not exist or is not readable.

See also `my-package-process-directory'."
  ...)
```

### Docstring Guidelines

1. **First line**: Complete sentence, under 80 characters
2. **Argument names**: UPPERCASE when mentioned
3. **Blank line**: After first sentence (for summary)
4. **Return value**: Document what function returns
5. **Signals**: Document errors that may be raised
6. **Examples**: Show typical usage
7. **See also**: Reference related functions

### Variable Docstrings

```elisp
(defvar my-package-timeout 30
  "Timeout in seconds for network operations.

This value controls how long to wait before giving up on
network requests.  A value of 0 means no timeout.

Changing this value affects all future operations but does
not impact currently running operations.")

(defvar my-package--internal-state nil
  "Internal state for my-package.
This variable should not be modified directly.")
```

### Custom Variables

```elisp
(defcustom my-package-auto-save t
  "Whether to automatically save after operations.

When non-nil, files are automatically saved after processing.
When nil, you must manually save changes.

You can also set this locally per buffer using:

  (setq-local my-package-auto-save nil)"
  :type 'boolean
  :safe #'booleanp
  :group 'my-package)

(defcustom my-package-backends '(backend1 backend2)
  "List of backends to use for processing.

Each backend should be a symbol recognized by my-package.
Backends are tried in order until one succeeds.

Available backends:
  backend1 - Fast but limited
  backend2 - Slower but more features
  backend3 - Requires external program"
  :type '(repeat (choice (const :tag "Backend 1" backend1)
                         (const :tag "Backend 2" backend2)
                         (const :tag "Backend 3" backend3)))
  :group 'my-package)
```

## Package Commentary

```elisp
;;; my-package.el --- Brief description  -*- lexical-binding: t -*-

;; [Headers...]

;;; Commentary:

;; My Package provides tools for processing data files.
;;
;; Features:
;;
;; - Process multiple file formats
;; - Batch processing support
;; - Customizable backends
;; - Integration with other tools
;;
;; Basic usage:
;;
;;     (require 'my-package)
;;     (my-package-enable)
;;     (my-package-process-file "data.csv")
;;
;; Configuration:
;;
;;     (setq my-package-auto-save nil)
;;     (setq my-package-timeout 60)
;;
;; For more information, see the Info manual:
;;
;;     C-h i m My Package RET
;;
;; Or visit the project page:
;;
;;     https://github.com/user/my-package

;;; Code:
```

## README.md

### Complete README Structure

```markdown
# My Package

[![MELPA](https://melpa.org/packages/my-package-badge.svg)](https://melpa.org/#/my-package)
[![CI](https://github.com/user/my-package/workflows/CI/badge.svg)](https://github.com/user/my-package/actions)

Brief one-sentence description of what the package does.

## Features

- Feature 1
- Feature 2
- Feature 3

## Installation

### MELPA

```elisp
(use-package my-package
  :ensure t
  :config
  (my-package-enable))
```

### Manual

Clone this repository:

```bash
git clone https://github.com/user/my-package.git
```

Add to your `init.el`:

```elisp
(add-to-list 'load-path "/path/to/my-package")
(require 'my-package)
```

## Usage

### Basic Usage

```elisp
;; Enable globally
(my-package-mode 1)

;; Or in specific buffers
(add-hook 'text-mode-hook #'my-package-mode)
```

### Commands

| Command | Description | Keybinding |
|---------|-------------|------------|
| `my-package-enable` | Enable the package | - |
| `my-package-process` | Process current buffer | `C-c m p` |
| `my-package-export` | Export to file | `C-c m e` |

### Configuration

```elisp
;; Set timeout
(setq my-package-timeout 60)

;; Choose backend
(setq my-package-backend 'backend2)

;; Disable auto-save
(setq my-package-auto-save nil)
```

## Customization

All options can be customized via:

```
M-x customize-group RET my-package RET
```

Key options:

- `my-package-timeout` - Operation timeout in seconds
- `my-package-auto-save` - Automatically save after processing
- `my-package-backend` - Which backend to use

## Examples

### Example 1: Process File

```elisp
(my-package-process-file "data.csv" t)
```

### Example 2: Batch Processing

```elisp
(my-package-process-directory "~/data/" "\\.csv$")
```

## Troubleshooting

### Package doesn't load

Make sure the package is in your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/my-package")
```

### Command not found

Ensure the package is loaded:

```elisp
(require 'my-package)
```

## Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Ensure all tests pass
5. Submit a pull request

## License

GPL-3.0-or-later

## Acknowledgments

- Thanks to contributor1
- Thanks to contributor2
```

## Info Manual

### Create Manual

**doc/my-package.texi**:

```texinfo
\input texinfo @c -*-texinfo-*-
@c %**start of header
@setfilename my-package.info
@settitle My Package Manual
@documentencoding UTF-8
@c %**end of header

@copying
This manual is for My Package version 1.0.

Copyright @copyright{} 2025 Your Name

@quotation
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation.
@end quotation
@end copying

@titlepage
@title My Package Manual
@subtitle For version 1.0
@author Your Name
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@contents

@node Top
@top My Package

My Package provides tools for processing data files.

@menu
* Introduction::      What is My Package?
* Installation::      Installing the package
* Usage::             How to use the package
* Configuration::     Customization options
* Index::             Complete index
@end menu

@node Introduction
@chapter Introduction

My Package helps you process data files efficiently.

@node Installation
@chapter Installation

Install from MELPA:

@lisp
(use-package my-package
  :ensure t)
@end lisp

@node Usage
@chapter Usage

@section Basic Usage

Enable the package:

@lisp
(my-package-enable)
@end lisp

@section Commands

@table @code
@item my-package-enable
Enable the package globally.

@item my-package-process-file
Process a single file.
@end table

@node Configuration
@chapter Configuration

@defvar my-package-timeout
Timeout in seconds for operations.
@end defvar

@defvar my-package-auto-save
Whether to auto-save after processing.
@end defvar

@node Index
@unnumbered Index

@printindex cp

@bye
```

### Build Info Manual

```bash
# Generate info file
makeinfo doc/my-package.texi -o my-package.info

# View
info -f my-package.info

# Install system-wide
sudo install-info my-package.info /usr/share/info/dir
```

### Include in Package

```elisp
;; In Eask
(package-file "my-package.el")
(files "my-package.el" "my-package-utils.el" "my-package.info")
```

## CHANGELOG.md

```markdown
# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- New feature in development

## [1.2.0] - 2025-01-15

### Added
- New command `my-package-export` for exporting data
- Support for CSV format
- Interactive completion for file selection

### Changed
- Improved performance of file processing (30% faster)
- Updated documentation with more examples
- Refactored backend system for extensibility

### Deprecated
- `my-package-old-function` - Use `my-package-new-function` instead

### Removed
- Removed support for deprecated format X

### Fixed
- Fixed bug in error handling (#42)
- Fixed memory leak in batch processing (#45)
- Corrected documentation typo (#48)

### Security
- Fixed potential security issue with file paths

## [1.1.0] - 2025-01-01

### Added
- Initial public release
- Basic file processing
- Customization options

## [1.0.0] - 2024-12-15

### Added
- Initial development version
```

## Documentation Tools

### checkdoc

```elisp
;; Check docstrings
M-x checkdoc

;; Check current buffer
M-x checkdoc-current-buffer

;; Fix automatically
M-x checkdoc-eval-current-buffer
```

### package-lint

```bash
# Check documentation
eask lint checkdoc
```

## Best Practices

### Docstrings

1. **Complete sentences**: Start with capital, end with period
2. **First line summary**: Complete thought, under 80 chars
3. **Document arguments**: Use UPPERCASE for arg names
4. **Document return**: What does function return?
5. **Document signals**: What errors can occur?
6. **Examples**: Show typical usage
7. **Cross-references**: Link to related functions

### README

1. **Clear title**: What is the package?
2. **Installation**: Simple, copy-paste instructions
3. **Usage examples**: Show common use cases
4. **Configuration**: List key options
5. **Screenshots**: If UI-heavy (optional)
6. **Badges**: MELPA, CI status
7. **Contributing**: How to help
8. **License**: Clear license information

### Info Manual

1. **Comprehensive**: More detail than README
2. **Organized**: Logical chapter structure
3. **Examples**: Lots of code examples
4. **Index**: Complete index for searching
5. **Cross-references**: Link related sections

## Resources

- [Docstring Tips](https://www.gnu.org/software/emacs/manual/html_node/elisp/Documentation-Tips.html)
- [Texinfo Manual](https://www.gnu.org/software/texinfo/manual/texinfo/)
- [Keep a Changelog](https://keepachangelog.com/)
- [README Template](https://github.com/othneildrew/Best-README-Template)
