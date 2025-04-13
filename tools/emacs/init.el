;;; init.el --- init configuration file -*- lexical-binding: t; -*-

;; Copyright (c) 2020-2023  Vincent Demeester <vincent@sbr.pm>

;; Author: Vincent Demeester <vincent@sbr.pm>
;; URL: https://git.sr.ht/~vdemeester/home
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1"))

;; This file is NOT part of GNU Emacs.

;; This file is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the
;; Free Software Foundation, either version 3 of the License, or (at
;; your option) any later version.
;;
;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this file.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; See my dotfiles: https://git.sr.ht/~vdemeester/home

;;; Code:


(defconst emacs-start-time (current-time))

(let ((minver 29))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(setq inhibit-default-init t)           ; Disable the site default settings

(setq confirm-kill-emacs #'y-or-n-p)
(setq initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Might not work as well on Windows but meh, I don't use it.
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-language-environment 'utf-8)
(set-selection-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)

(setq custom-file (locate-user-emacs-file "custom.el"))
(setq
   custom-buffer-done-kill nil          ; Kill when existing
   custom-buffer-verbose-help nil       ; Remove redundant help text
   custom-unlispify-tag-names nil       ; Show me the real variable name
   custom-unlispify-menu-entries nil)
;; Create the custom-file if it doesn't exists
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file :no-error-if-file-is-missing)

(setq echo-keystrokes 0.1) ;; display command keystrokes quickly

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-h h"))

;; Disable owerwrite-mode, iconify-frame and diary
(mapc
 (lambda (command)
   (put command 'disabled t))
 '(overwrite-mode iconify-frame diary))
;; And enable those commands (disabled by default)
(mapc
 (lambda (command)
   (put command 'disabled nil))
 '(list-timers narrow-to-region narrow-to-page upcase-region downcase-region))

(defun prot/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
   ((region-active-p)
    (keyboard-quit))
   ((derived-mode-p 'completion-list-mode)
    (delete-completion-window))
   ((> (minibuffer-depth) 0)
    (abort-recursive-edit))
   (t
    (keyboard-quit))))

(define-key global-map (kbd "C-g") #'prot/keyboard-quit-dwim)

(add-to-list 'load-path (concat user-emacs-directory "/lisp/"))
(add-to-list 'load-path (concat user-emacs-directory "/lisp/aider.el"))
(add-to-list 'load-path (concat user-emacs-directory "/lisp/auto-side-windows"))
(add-to-list 'load-path (concat user-emacs-directory "/lisp/consult-mu"))
(add-to-list 'load-path (concat user-emacs-directory "/lisp/consult-mu/extras"))
(add-to-list 'load-path (concat user-emacs-directory "/config/"))

(unless noninteractive
  (defconst font-height 130
    "Default font-height to use.")
  ;; 2024-10-05: Switching from Ubuntu Mono to Cascadia Mono
  ;; 2024-96-06: Switching from Cascadia Mono to JetBrains Mono
  (defconst font-family-mono "JetBrains Mono"
    "Default monospace font-family to use.")
  (defconst font-family-sans "Ubuntu Sans"
    "Default sans font-family to use.")
  ;; Middle/Near East: שלום, السّلام عليكم
  (when (member "Noto Sans Arabic" (font-family-list))
    (set-fontset-font t 'arabic "Noto Sans Arabic"))
  (when (member "Noto Sans Hebrew" (font-family-list))
    (set-fontset-font t 'arabic "Noto Sans Hebrew"))
  ;; Africa: ሠላም
  (when (member "Noto Sans Ethiopic" (font-family-list))
    (set-fontset-font t 'ethiopic "Noto Sans Ethiopic"))

  ;; If font-family-mono or font-family-sans are not available, use the default Emacs face
  (set-face-attribute 'default nil
		      :family font-family-mono
		      :height font-height
		      :weight 'regular)
  (set-face-attribute 'fixed-pitch nil
		      :family font-family-mono
		      :weight 'medium
		      :height font-height)
  (set-face-attribute 'variable-pitch nil
		      :family font-family-sans
		      :weight 'regular)

  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append)

  (defvar contrib/after-load-theme-hook nil
    "Hook run after a color theme is loaded using `load-theme'.")

  (defun contrib/run-after-load-theme-hook (&rest _)
    "Run `contrib/after-load-theme-hook'."
    (run-hooks 'contrib/after-load-theme-hook))

  (advice-add #'load-theme :after #'contrib/run-after-load-theme-hook)

  (require 'modus-themes)
  (setq modus-themes-to-toggle '(modus-operandi modus-vivendi)
	modus-themes-slanted-constructs nil
	modus-themes-italic-constructs nil
	modus-themes-bold-constructs nil
	modus-themes-mixed-fonts t
	modus-themes-subtle-diffs t
	modus-themes-fringes 'subtle ; {nil,'subtle,'intense}
	modus-themes-headings '((0 . (variable-pitch semilight 1.5))
				(1 . (regular 1.4))
				(2 . (regular 1.3))
				(3 . (regular 1.2))
				(agenda-structure . (variable-pitch light 2.2))
				(agenda-date . (variable-pitch regular 1.3))
				(t . (regular 1.15)))
	modus-themes-intense-paren-match t
	modus-themes-completions '(opinionated) ; {nil,'moderate,'opinionated}
	modus-themes-diffs 'desaturated ; {nil,'desaturated,'fg-only}
	modus-themes-org-blocks 'gray-background
	modus-themes-paren-match '(subtle-bold)
	modus-themes-variable-pitch-headings nil
	modus-themes-rainbow-headings t
	modus-themes-section-headings nil
	modus-themes-scale-headings t
	)

  (defun my-update-active-mode-line-colors ()
    (set-face-attribute
     'mode-line nil
     :foreground (modus-themes-get-color-value 'fg-mode-line-active)
     :background (modus-themes-get-color-value 'bg-blue-nuanced)))
  (add-hook 'modus-themes-after-load-theme-hook #'my-update-active-mode-line-colors)
  (define-key global-map (kbd "C-<f5>") #'modus-themes-toggle)

  (load-theme 'modus-operandi :no-confirm)
  (my-update-active-mode-line-colors))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

;; Init `delight'
;; (unless (package-installed-p 'delight)
;;   (package-refresh-contents)
;;   (package-install 'delight))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

;; (unless (package-installed-p 'use-package)
;;   (package-refresh-contents)
;;   (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(setenv "SSH_AUTH_SOCK" "/run/user/1000/yubikey-agent/yubikey-agent.sock")
;; (setenv "SSH_AUTH_SOCK" "/run/user/1000/gnupg/S.gpg-agent.ssh")

(defconst vde/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

;; Remove built-in org-mode
(require 'cl-seq)
(setq load-path
      (cl-remove-if
       (lambda (x)
         (string-match-p "org$" x))
       load-path))

;; 2024-07-12: I wonder if I should be explicit instead, as using
;; `require' explicitly. The benefit would be that I decide the order
;; they load instead of relying on file-system.
;; (vde/el-load-dir (concat user-emacs-directory "/config/"))
(require 'init-func)
(require 'org-func)
(require 'project-func)

;; Make native compilation silent and prune its cache.
(when (native-comp-available-p)
  (setq native-comp-async-report-warnings-errors 'silent) ; Emacs 28 with native compilation
  (setq native-compile-prune-cache t)
  (setq native-comp-jit-compilation t)
  (setq native-comp-async-query-on-exit t)) ; Emacs 29

(setq byte-compile-warnings '(not free-vars unresolved noruntime lexical make-local))

;; Refactor this completely. Reduce to the minimum.
(unless noninteractive
  (require '00-clean) ;; Maybe refactor no-littering
  (require 'config-editing)
  (require 'config-files)
  (require 'config-misc)
  (require 'config-keybindings)
  (require 'config-appearance)
  (require 'config-buffers)
  (require 'config-compile)
  (require 'config-completion)
  (require 'config-dired)
  (require 'config-mouse)
  (require 'config-navigating)
  (require 'config-org)
  (require 'config-programming)
  (require 'config-projects)
  (require 'config-search)
  (require 'config-shells)
  (require 'config-vcs)
  (require 'config-web)
  (require 'config-windows)
  (require 'config-llm)
  (require 'programming-config)
  (require 'programming-containers)
  (require 'programming-cue)
  (require 'programming-elisp)
  (require 'programming-eglot)
  (require 'programming-go)
  (require 'programming-js)
  (require 'programming-nix)
  (require 'programming-treesitter)
  (require 'programming-web)
  (require 'config-mu4e))

(if (file-exists-p (downcase (concat user-emacs-directory "/hosts/" (vde/short-hostname) ".el")))
    (load-file (downcase (concat user-emacs-directory "/hosts/" (vde/short-hostname) ".el"))))

(let ((elapsed (float-time (time-subtract (current-time)
                                          emacs-start-time))))
  (message "Loading %s...done (%.3fs)" load-file-name elapsed))

(add-hook 'after-init-hook
          `(lambda ()
             (let ((elapsed
                    (float-time
                     (time-subtract (current-time) emacs-start-time))))
               (message "Loading %s...done (%.3fs) [after-init]"
                        ,load-file-name elapsed))) t)
