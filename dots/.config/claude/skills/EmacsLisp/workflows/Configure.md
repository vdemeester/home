# Configure Workflow

Modern Emacs configuration using use-package, package managers, and best practices for Emacs 29+.

## Modern Configuration (2025)

### Recommended Approach

**Emacs 29+**: use-package is built-in
- No need to install use-package separately
- Clean, declarative package configuration
- Lazy loading for fast startup

## Configuration Structure

```
~/.emacs.d/
├── early-init.el      # Early initialization (Emacs 27+)
├── init.el            # Main configuration
├── custom.el          # Custom-set variables (optional)
└── lisp/              # Custom Elisp files
    └── my-functions.el
```

## early-init.el

Runs before package initialization and GUI setup:

```elisp
;;; early-init.el --- Early initialization  -*- lexical-binding: t -*-

;; Disable package.el in favor of straight.el (optional)
;; Or keep it for use-package with package.el
(setq package-enable-at-startup t)

;; Improve startup time
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Disable unnecessary UI elements early
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Prevent unwanted runtime compilation
(setq native-comp-deferred-compilation nil)

;;; early-init.el ends here
```

## init.el (Modern with use-package)

### Basic Setup

```elisp
;;; init.el --- Emacs configuration  -*- lexical-binding: t -*-

;; Reset GC threshold after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1)))

;; Package archives
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)
(package-initialize)

;; Ensure use-package is available
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)  ; Auto-install packages

;; Keep customizations in separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

;;; init.el ends here
```

### Essential Packages

```elisp
;; Magit - Git interface
(use-package magit
  :bind ("C-x g" . magit-status)
  :config
  (setq magit-display-buffer-function
        #'magit-display-buffer-same-window-except-diff-v1))

;; Company - Completion
(use-package company
  :hook (prog-mode . company-mode)
  :custom
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous)))

;; Vertico - Minibuffer completion
(use-package vertico
  :init
  (vertico-mode 1)
  :custom
  (vertico-cycle t))

;; Marginalia - Rich annotations
(use-package marginalia
  :init
  (marginalia-mode 1))

;; Orderless - Flexible matching
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion)))))

;; Which-key - Key binding help
(use-package which-key
  :init
  (which-key-mode 1)
  :custom
  (which-key-idle-delay 0.5))
```

### Programming

```elisp
;; LSP (choose one: eglot or lsp-mode)

;; Eglot (built-in since Emacs 29)
(use-package eglot
  :ensure nil  ; Built-in
  :hook ((python-mode . eglot-ensure)
         (go-mode . eglot-ensure)
         (rust-mode . eglot-ensure)))

;; OR lsp-mode (more features, heavier)
(use-package lsp-mode
  :hook ((python-mode . lsp)
         (go-mode . lsp))
  :commands lsp
  :custom
  (lsp-headerline-breadcrumb-enable nil))

;; Tree-sitter (built-in since Emacs 29)
(use-package treesit
  :ensure nil
  :config
  (setq treesit-language-source-alist
        '((python "https://github.com/tree-sitter/tree-sitter-python")
          (rust "https://github.com/tree-sitter/tree-sitter-rust")
          (go "https://github.com/tree-sitter/tree-sitter-go"))))
```

### Org Mode

```elisp
(use-package org
  :ensure nil  ; Built-in
  :custom
  (org-directory "~/org")
  (org-agenda-files '("~/org/agenda.org"))
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t))))

(use-package org-roam
  :custom
  (org-roam-directory "~/org/roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode))
```

## Alternative: straight.el

For reproducible configuration with pinned versions:

### Bootstrap

```elisp
;;; init.el with straight.el

;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

;; Now use packages normally
(use-package magit)
(use-package company
  :hook (prog-mode . company-mode))
```

### Lockfile

straight.el creates `straight/versions/default.el` with pinned versions:

```elisp
;; Example lockfile entry
(("magit" . "abc123...")
 ("company" . "def456..."))
```

Commit this file for reproducible installs.

## Performance Optimization

### Startup Time

```elisp
;; Measure startup time
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;; Lazy load packages
(use-package markdown-mode
  :defer t
  :mode "\\.md\\'")

(use-package yaml-mode
  :defer t
  :mode "\\.ya?ml\\'")

;; Defer non-essential packages
(use-package dashboard
  :defer 1  ; Load 1 second after startup
  :config
  (dashboard-setup-startup-hook))
```

### Native Compilation

Emacs 28+ with native compilation:

```elisp
;; Enable native compilation
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-async-report-warnings-errors nil)
  (setq native-comp-deferred-compilation t))
```

## Configuration Patterns

### Theme

```elisp
(use-package modus-themes
  :ensure nil  ; Built-in since Emacs 28
  :config
  (setq modus-themes-bold-constructs t
        modus-themes-italic-constructs t)
  (load-theme 'modus-vivendi t))

;; Or external theme
(use-package doom-themes
  :config
  (load-theme 'doom-one t))
```

### Keybindings

```elisp
;; Global keybindings
(global-set-key (kbd "C-c f") #'find-file)
(global-set-key (kbd "C-c b") #'switch-to-buffer)

;; With use-package
(use-package ace-window
  :bind ("M-o" . ace-window))

;; Define custom keymap
(defvar my-leader-map (make-sparse-keymap)
  "Personal leader keymap.")

(global-set-key (kbd "C-c m") my-leader-map)
(define-key my-leader-map (kbd "f") #'find-file)
(define-key my-leader-map (kbd "b") #'switch-to-buffer)
```

### Custom Functions

```elisp
;; In lisp/my-functions.el
(defun my/open-config ()
  "Open init.el."
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

(defun my/reload-config ()
  "Reload init.el."
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

(global-set-key (kbd "C-c e c") #'my/open-config)
(global-set-key (kbd "C-c e r") #'my/reload-config)

;; Load custom functions
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'my-functions)
```

## Debugging Configuration

### Profile Startup

```bash
# Built-in profiler
emacs --debug-init

# Or use esup (Emacs Start Up Profiler)
M-x package-install RET esup RET
M-x esup
```

### use-package Debug

```elisp
;; Show what use-package does
(setq use-package-verbose t)
(setq use-package-compute-statistics t)

;; View statistics
M-x use-package-report
```

## Migration from Old Config

### From require to use-package

```elisp
;; Old way
(add-to-list 'load-path "~/.emacs.d/lisp/some-package")
(require 'some-package)
(setq some-package-variable value)
(global-set-key (kbd "C-c s") #'some-package-command)

;; New way
(use-package some-package
  :load-path "lisp/some-package"
  :custom
  (some-package-variable value)
  :bind ("C-c s" . some-package-command))
```

## Best Practices

1. **Use lexical binding**: `;;; -*- lexical-binding: t -*-`
2. **Lazy load packages**: Use `:defer`, `:hook`, or `:mode`
3. **Keep customizations separate**: Use `custom-file`
4. **Organize by category**: Group related packages
5. **Comment your config**: Explain non-obvious settings
6. **Version control**: Keep `.emacs.d` in git
7. **Measure startup time**: Keep it under 2 seconds
8. **Use built-in features**: Prefer built-in over external packages
9. **Pin critical packages**: Use straight.el for important configs
10. **Test incrementally**: Add packages one at a time

## Example Complete Configuration

### Minimal Modern Config

```elisp
;;; init.el --- Minimal modern Emacs config  -*- lexical-binding: t -*-

;; Package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; use-package (built-in Emacs 29+)
(require 'use-package)
(setq use-package-always-ensure t)

;; UI improvements
(setq inhibit-startup-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode 1)
(global-hl-line-mode 1)
(global-display-line-numbers-mode 1)

;; Editing
(setq-default indent-tabs-mode nil
              tab-width 4)
(electric-pair-mode 1)
(delete-selection-mode 1)
(save-place-mode 1)
(global-auto-revert-mode 1)

;; Essential packages
(use-package magit
  :bind ("C-x g" . magit-status))

(use-package vertico
  :init (vertico-mode))

(use-package which-key
  :init (which-key-mode))

;; Theme
(use-package modus-themes
  :ensure nil
  :config
  (load-theme 'modus-vivendi t))

;;; init.el ends here
```

## Resources

- [use-package Documentation](https://github.com/jwiegley/use-package)
- [straight.el](https://github.com/radian-software/straight.el)
- [Emacs Package Manager Comparison](https://www.reddit.com/r/emacs/wiki/packages)
- [Sacha Chua's Emacs Config](https://sachachua.com/dotemacs/)
