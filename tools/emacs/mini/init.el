;;; init --- vdemeester's emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vincent Demeester
;; Author: Vincent Demeester <vincent@sbr.pm>

;; This file is NOT part of GNU Emacs.
;;; Commentary:
;; This is the "mini" version for now, but aims to become the default one.
;;; Code:

;;; Some constants I am using across the configuration.
(defconst org-directory "~/desktop/org/"
  "`org-mode' directory, where most of the org-mode file lives.")
(defconst org-notes-directory (expand-file-name "notes" org-directory)
  "`org-mode' notes directory, for notes obviously, most likely managed by denote.")
(defconst org-inbox-file (expand-file-name "inbox.org" org-directory)
  "`org-mode' inbox file, where we collect entries to be triaged.")
(defconst org-todos-file (expand-file-name "todos.org" org-directory)
  "`org-mode' file for TODOs.  This is the main file for the org angenda entries.")
(defconst org-journal-file (expand-file-name "journal.org" org-directory)
  "`org-mode' journal file, for journal-ling.")
(defconst org-archive-dir (expand-file-name "archive" org-directory)
  "`org-mode' directory of archived files.")
(defconst org-people-dir (expand-file-name "people" org-notes-directory)
  "`org-mode' people files directory, most likely managed by denote.")

;;; The configuration.

;;; Quick access to certain key file using registers
(set-register ?e `(file . ,(locate-user-emacs-file "init.el")))
(set-register ?i `(file . ,org-inbox-file))
(set-register ?t `(file . ,org-todos-file))
(set-register ?j `(file . ,org-journal-file))
(set-register ?o `(file . ,org-directory))
(set-register ?n `(file . ,org-notes-directory))
(set-register ?P `(file . ,org-people-dir))

;;; Some GC optimizations
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000000))

(setq gc-cons-threshold most-positive-fixnum)

(run-with-idle-timer 1.2 t 'garbage-collect)

(defconst emacs-start-time (current-time))

(let ((minver 29))
  (unless (>= emacs-major-version minver)
    (error "Your Emacs is too old -- this configuration requires v%s or higher" minver)))

(setq inhibit-default-init t)           ; Disable the site default settings

(setq confirm-kill-emacs #'y-or-n-p)

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

  (require 'modus-themes)
  (setopt modus-themes-common-palette-overrides modus-themes-preset-overrides-cooler
	  modus-themes-to-rotate '(modus-operandi modus-vivendi)
	  modus-themes-mixed-fonts t
	  modus-themes-headings '((0 . (variable-pitch semilight 1.5))
				  (1 . (regular 1.4))
				  (2 . (regular 1.3))
				  (3 . (regular 1.2))
				  (agenda-structure . (variable-pitch light 2.2))
				  (agenda-date . (variable-pitch regular 1.3))
				  (t . (regular 1.15))))
  ;; Default modus-operandi on GUI and modus-vivendi on CLI
  (if (display-graphic-p)
      (load-theme 'modus-operandi :no-confirm)
    (load-theme 'modus-vivendi :no-confirm)))

(setopt load-prefer-newer t)              ; Always load newer compiled files
(setopt ad-redefinition-action 'accept)   ; Silence advice redefinition warnings
(setopt debug-on-error t)
(setopt byte-compile-debug t)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics t))

(eval-when-compile
  (require 'use-package))

(use-package emacs
  :bind
  ("C-x m" . mark-defun)
  ("C-x C-b" . bs-show)
  ("M-o" . other-window)
  ("M-j" . duplicate-dwim)
  ;; (:map completion-preview-active-mode-map
  ;; ("M-n" . #'completion-preview-next-candidate)
  ;; ("M-p" . #'completion-preview-prev-candidate))
  :custom
  (create-lockfiles nil)   ; No backup files
  (make-backup-files nil)  ; No backup files
  (backup-inhibited t)     ; No backup files
  (tab-always-indent 'complete)
  (enable-local-variables :all)
  (select-enable-clipboard t)
  (select-enable-primary t)
  (comment-multi-line t)
  (make-backup-files nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (mouse-autoselect 1)
  (completion-cycle-threshold 2)
  (completion-ignore-case t)
  (completion-show-inline-help nil)
  (completions-detailed t)
  (enable-recursive-minibuffers t)
  (read-buffer-completion-ignore-case t)
  (read-file-name-completion-ignore-case t)
  (find-ls-option '("-exec ls -ldh {} +" . "-ldh"))  ; find-dired results with human readable sizes
  (switch-to-buffer-obey-display-actions t)
  :hook
  (after-init . global-hl-line-mode)
  (after-init . global-completion-preview-mode)
  (after-init . auto-insert-mode)
  (after-init . pixel-scroll-mode)
  :config
  (display-time-mode -1)
  (tooltip-mode -1)
  (blink-cursor-mode -1)
  (setenv "GIT_EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  (setenv "EDITOR" (format "emacs --init-dir=%s " (shell-quote-argument user-emacs-directory)))
  (delete-selection-mode 1)
  (defun er-keyboard-quit ()
    "Smater version of the built-in `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it."
    (interactive)
    (if (active-minibuffer-window)
	(if (minibufferp)
            (minibuffer-keyboard-quit)
          (abort-recursive-edit))
      (keyboard-quit)))
  (global-set-key [remap keyboard-quit] #'er-keyboard-quit))

(use-package passage
  :commands (passage-get))

(use-package ffap
  :hook
  (after-init . ffap-bindings))

(use-package icomplete
  :unless noninteractive
  :hook
  (icomplete-minibuffer-setup
   . (lambda()(interactive) 
       (setq-local completion-styles '(flex partial-completion initials basic))))
  (after-init . fido-vertical-mode)
  :custom
  (icomplete-compute-delay 0.01))

(use-package display-line-numbers
  :unless noninteractive
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (defun vde/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode)))
  :bind ("<f7>" . vde/toggle-line-numbers))

(use-package helpful
  :unless noninteractive
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h M" . helpful-macro)
         ("C-c h S" . helpful-at-point)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h C" . helpful-command)))

(use-package flymake
  :bind
  ("C-c f b" . flymake-show-buffer-diagnostics)
  :hook
  (prog-mode . flymake-mode))

(use-package aggressive-indent
  :commands (aggressive-indent-mode)
  :hook
  (emacs-lisp-mode . aggressive-indent-mode))

(use-package save-place
  :defer 1
  :config (save-place-mode 1))

(use-package symbol-overlay
  :custom
  (symbol-overlay-idle-time 0.2)
  :bind
  ("M-s s i" . symbol-overlay-put)
  ("M-N" . symbol-overlay-jump-next)
  ("M-P" . symbol-overlay-jump-prev)
  ("M-s s r" . symbol-overlay-rename)
  ("M-s s c" . symbol-overlay-remove-all)
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package savehist
  :unless noninteractive
  :hook (after-init . savehist-mode)
  :custom
  (history-length 10000)
  (savehist-save-minibuffer-history t)
  (savehist-delete-duplicates t)
  (savehist-autosave-interval 180)
  (savehist-additional-variables '(extended-command-history
				   search-ring
				   regexp-search-ring
				   comint-input-ring
				   compile-history
				   last-kbd-macro
				   shell-command-history)))

(use-package which-key
  :custom
  (which-key-separator " → " )
  (which-key-prefix-prefix "… ")
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  (which-key-idle-delay 1)
  (which-key-idle-secondary-delay 0.25)
  :hook
  (after-init . which-key-mode)
  :config
  
  ;; Define custom, concise descriptions for `tab-bar` commands under "C-x t"
  (which-key-add-key-based-replacements
    "C-c !"     "flymake"
    "C-x t C-f" "Open file in new tab"
    "C-x t RET" "Switch tabs"
    "C-x t C-r" "Open file (read-only) in new tab"
    "C-x t 0"   "Close current tab"
    "C-x t 1"   "Close other tabs"
    "C-x t 2"   "New empty tab"
    "C-x t G"   "Group tabs"
    "C-x t M"   "Move tab to position"
    "C-x t N"   "New tab and switch to it"
    "C-x t O"   "Previous tab"
    "C-x t b"   "Switch buffer in new tab"
    "C-x t d"   "Dired in new tab"
    "C-x t f"   "Open file in new tab"
    "C-x t m"   "Move tab left/right"
    "C-x t n"   "Duplicate tab"
    "C-x t o"   "Next tab"
    "C-x t p"   "Project in new tab"
    "C-x t r"   "Rename tab"
    "C-x t t"   "Switch to other tab"
    "C-x t u"   "Undo tab close"
    "C-x t ^ f" "Detach tab window"
    "C-x 8" "insert-special"
    "C-x 8 ^" "superscript (⁰, ¹, ², …)"
    "C-x 8 _" "subscript (₀, ₁, ₂, …)"
    "C-x 8 a" "arrows & æ (←, →, ↔, æ)"
    "C-x 8 e" "emojis (🫎, 🇧🇷, 🇮🇹, …)"
    "C-x 8 *" "common symbols ( , ¡, €, …)"
    "C-x 8 =" "macron (Ā, Ē, Ḡ, …)"
    "C-x 8 N" "macron (№)"
    "C-x 8 O" "macron (œ)"
    "C-x 8 ~" "tilde (~, ã, …)"
    "C-x 8 /" "stroke (÷, ≠, ø, …)"
    "C-x 8 ." "dot (·, ż)"
    "C-x 8 ," "cedilla (¸, ç, ą, …)"
    "C-x 8 '" "acute (á, é, í, …)"
    "C-x 8 `" "grave (à, è, ì, …)"
    "C-x 8 \"" "quotation/dieresis (\", ë, ß, …)"
    "C-x 8 1" "†, 1/…"
    "C-x 8 2" "‡"
    "C-x 8 3" "3/…"
    "C-x 4" "other-window"
    "C-x 5" "other-frame"))

(use-package newcomment
  :unless noninteractive
  :custom
  (comment-empty-lines t)
  (comment-fill-column nil)
  (comment-multi-line t)
  (comment-style 'multi-line)
  :config
  (defun prot/comment-dwim (&optional arg)
    "Alternative to `comment-dwim': offers a simple wrapper
    around `comment-line' and `comment-dwim'.

    If the region is active, then toggle the comment status of the
    region or, if the major mode defines as much, of all the lines
    implied by the region boundaries.

    Else toggle the comment status of the line at point."
    (interactive "*P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg))))
  :bind (("C-;" . prot/comment-dwim)
    ("C-:" . comment-kill)
    ("M-;" . comment-indent)
    ("C-x C-;" . comment-box)))

(use-package dired
  :custom
  (dired-hide-details-hide-information-lines 'nil)
  (dired-kill-when-opening-new-dired-buffer 't)
  :bind
  (:map dired-mode-map
	("E"   . wdired-change-to-wdired-mode)
	("l"   . dired-find-file))
  :hook
  (dired-mode . dired-omit-mode)
  (dired-mode . dired-hide-details-mode)
  (dired-mode . dired-sort-toggle-or-edit))

(use-package alert
  :defer 2
  :init
  (defun alert-after-finish-in-background (buf str)
    (when (or (not (get-buffer-window buf 'visible)) (not (frame-focus-state)))
      (alert str :buffer buf)))
  :config
  (setq alert-default-style 'libnotify))

(use-package elec-pair
  :hook (after-init-hook . electric-pair-mode))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'forward)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

(use-package compile
  :unless noninteractive
  :commands (compile)
  :custom
  (compilation-always-kill t)
  (compilation-scroll-output t)
  (ansi-color-for-compilation-mode t)
  :config
  (add-hook 'compilation-finish-functions #'alert-after-finish-in-background))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

;; Recentf
(use-package recentf
  :defer t
  :hook
  (after-nit . recentf-mode)
  :bind (("C-x C-r" . recentf-open)))

(use-package prog-mode
  :hook
  (prog-mode . eldoc-mode)
  :custom
  (eldoc-idle-delay 0.2))

(use-package eglot
  :bind
  (:map eglot-mode-map
        ("C-c e a" . eglot-code-actions)
        ("C-c e r" . eglot-reconnect)
        ("<f2>" . eglot-rename)
        ("C-c e ?" . eldoc-print-current-symbol-info))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  :config
  (add-to-list 'eglot-ignored-server-capabilities :documentHighlightProvider)
  (add-to-list 'eglot-server-programs `(json-mode  "vscode-json-language-server" "--stdio"))
  (add-to-list 'eglot-server-programs '(nix-mode . ("nil")))
  (setq-default eglot-workspace-configuration
		'(
		  :gopls (
			  :usePlaceholders t
			  ;; See https://github.com/golang/tools/blob/master/gopls/doc/analyzers.md
			  :analyses (
				     :QF1006 t
				     :QF1007 t
				     :S1002 t
				     :S1005 t
				     :S1006 t
				     :S1008 t
				     :S1025 t
				     :SA1003 t
				     :SA1014 t
				     :SA1015 t
				     :SA1023 t
				     :SA1032 t
				     :SA2002 t
				     :SA4023 t
				     :SA4031 t
				     :SA5000 t
				     :SA5010 t
				     :SA5000 t
				     :SA6000 t
				     :SA6001 t
				     :SA6002 t
				     :SA6003 t
				     :SA9003 t
				     :SA9007 t
				     :ST1000 t
				     :ST1001 t
				     :ST1005 t
				     :ST1013 t
				     :ST1015 t
				     :ST1016 t
				     :ST1017 t
				     :ST1019 t
				     :ST1020 t
				     :ST1021 t
				     :ST1022 t
				     :ST1023 t
				     :shadow t
				     )
			  ;; See https://github.com/golang/tools/blob/master/gopls/doc/inlayHints.md
			  :hints (:constantValues t :compositeLiteralTypes t :compositeLiteralFields t))
		  :nil (
			:formatting (:command ["nixfmt"])
			:nix (
			      :maxMemoryMB 2560
			      :autoEvalInputs t
			      :nixpkgsInputName "nixpkgs"
			      )
			)
		  :pylsp (
			  :configurationSources ["flake8"]
			  :plugins (:pycodestyle (:enabled nil)
						 :black (:enabled t)
						 :mccabe (:enabled nil)
						 :flake8 (:enabled t)))))
  (defun eglot-format-buffer-on-save ()
    (if (and (project-current) (eglot-managed-p))
        (add-hook 'before-save-hook #'eglot-format-buffer nil 'local)
      (remove-hook 'before-save-hook #'eglot-format-buffer 'local)))
  (add-hook 'eglot-managed-mode-hook #'eglot-format-buffer-on-save)
  :hook
  ;; (before-save . gofmt-before-save)
  ;; (before-save . eglot-format-buffer)
  (nix-mode . eglot-ensure)
  (nix-ts-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  (python-ts-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (sh-script-mode . eglot-ensure))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(go-mode . go-ts-mode)))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package yaml-ts-mode
  :mode "\\.yaml\\'")

(use-package go-ts-mode
  :mode (("\\.go$" . go-ts-mode)
         ("\\.go" . go-ts-mode)
         ("\\.go\\'" . go-ts-mode)))

(use-package nix-ts-mode
  :if (executable-find "nix")
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :if (executable-find "nix")
  :after nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :if (executable-find "nix")
  :after nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nixpkgs-fmt
  :if (executable-find "nix")
  :after nix-ts-mode
  :custom
  (nixpkgs-fmt-command "nixfmt")
  :config
  (add-hook 'nix-ts-mode-hook 'nixpkgs-fmt-on-save-mode))

(use-package minions
  :hook (after-init . minions-mode)
  :config
  (add-to-list 'minions-prominent-modes 'flymake-mode))

(use-package vundo
  :bind (("M-u"   . undo)
         ("M-U"   . undo-redo)
         ("C-x u" . vundo)))

(use-package vde-vcs
  :commands (vde/gh-get-current-repo vde/vc-browse-remote)
  :bind (("C-x v B" . vde/vc-browse-remote)))

(use-package project-func
  :commands (vde/project-magit-status vde/project-eat vde/project-vterm vde/project-run-in-vterm vde/project-try-local vde/open-readme))

(use-package project
  :commands (project-find-file project-find-regexp)
  :custom
  (project-switch-commands '((?f "File" project-find-file)
			     (?g "Grep" project-find-regexp)
			     (?d "Dired" project-dired)
			     (?b "Buffer" project-switch-to-buffer)
			     (?q "Query replace" project-query-replace-regexp)
			     (?m "Magit" vde/project-magit-status)
			     (?e "Eshell" project-eshell)
			     (?E "Eat" vde/project-eat)
			     (?s "Vterm" vde/project-vterm)
			     (?R "README" vde/open-readme)
			     (?g "Checkout GitHub PR" checkout-github-pr)))
  (project-mode-line t)
  (project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (project-vc-extra-root-markers '(".project" "Cargo.toml" "pyproject.toml" "requirements.txt" "go.mod"))
  :bind
  ("C-x p v" . vde/project-magit-status)
  ("C-x p s" . vde/project-vterm)
  ("C-x p X" . vde/project-run-in-vterm)
  ("C-x p E" . vde/project-eat)
  ("C-x p G" . checkout-github-pr)
  ("C-x p F" . flymake-show-project-diagnostics))

(use-package magit
  :unless noninteractive
  :commands (magit-status magit-clone magit-pull magit-blame magit-log-buffer-file magit-log)
  :bind (("C-c v c" . magit-commit)
         ("C-c v C" . magit-checkout)
         ("C-c v b" . magit-branch)
         ("C-c v d" . magit-dispatch)
         ("C-c v f" . magit-fetch)
         ("C-c v g" . magit-blame)
         ("C-c v l" . magit-log-buffer-file)
         ("C-c v L" . magit-log)
         ("C-c v p" . magit-pull)
         ("C-c v P" . magit-push)
         ("C-c v r" . magit-rebase)
	 ("C-c v s" . magit-stage)
         ("C-c v v" . magit-status))
  :custom
  (magit-save-repository-buffers 'dontask)
  (magit-refs-show-commit-count 'all)
  (magit-branch-prefer-remote-upstream '("main"))
  (magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-refresh-status-buffer nil)
  :config
  ;; cargo-culted from https://github.com/magit/magit/issues/3717#issuecomment-734798341
  ;; valid gitlab options are defined in https://docs.gitlab.com/ee/user/project/push_options.html
  ;;
  ;; the second argument to transient-append-suffix is where to append
  ;; to, not sure what -u is, but this works
  (transient-append-suffix 'magit-push "-u"
    '(1 "=s" "Skip gitlab pipeline" "--push-option=ci.skip"))
  (transient-append-suffix 'magit-push "=s"
    '(1 "=m" "Create gitlab merge-request" "--push-option=merge_request.create"))
  (transient-append-suffix 'magit-push "=m"
    '(1 "=o" "Set push option" "--push-option="))  ;; Will prompt, can only set one extra
  )

(use-package ediff
  :commands (ediff ediff-files ediff-merge ediff3 ediff-files3 ediff-merge3)
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-diff-options "-w")
  :hook
  (ediff-after-quit-hook-internal . winner-undo))

(use-package diff
  :custom
  (diff-default-read-only nil)
  (diff-advance-after-apply-hunk t)
  (diff-update-on-the-fly t)
  (diff-refine 'font-lock)
  (diff-font-lock-prettify nil)
  (diff-font-lock-syntax nil))

(use-package gitconfig-mode
  :commands (gitconfig-mode)
  :mode (("/\\.gitconfig\\'"  . gitconfig-mode)
         ("/\\.git/config\\'" . gitconfig-mode)
         ("/git/config\\'"    . gitconfig-mode)
         ("/\\.gitmodules\\'" . gitconfig-mode)))

(use-package gitignore-mode
  :commands (gitignore-mode)
  :mode (("/\\.gitignore\\'"        . gitignore-mode)
         ("/\\.git/info/exclude\\'" . gitignore-mode)
         ("/git/ignore\\'"          . gitignore-mode)))

(use-package gitattributes-mode
  :commands (gitattributes-mode)
  :mode (("/\\.gitattributes" . gitattributes-mode)))

(use-package diff-hl
  :hook (find-file . diff-hl-mode)
  :hook (prog-mode . diff-hl-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh)
  :bind
  (:map diff-hl-command-map
	("n" . diff-hl-next-hunk)
	("p" . diff-hl-previous-hunk)
	("[" . nil)
	("]" . nil)
	("DEL"   . diff-hl-revert-hunk)
	("<delete>" . diff-hl-revert-hunk)
	("SPC" . diff-hl-mark-hunk)
	:map vc-prefix-map
	("n" . diff-hl-next-hunk)
	("p" . diff-hl-previous-hunk)
	("s" . diff-hl-stage-dwim)
	("DEL"   . diff-hl-revert-hunk)
	("<delete>" . diff-hl-revert-hunk)
	("SPC" . diff-hl-mark-hunk))
  :config
  (put 'diff-hl-inline-popup-hide
       'repeat-map 'diff-hl-command-map))

(use-package diff-hl-inline-popup
  :after (diff-hl))
(use-package diff-hl-show-hunk
  :after (diff-hl))

(use-package diff-hl-dired
  :after (diff-hl)
  :hook (dired-mode . diff-hl-dired-mode))

(use-package corfu
  :custom
  (corfu-auto 't)
  :bind
  (:map corfu-map
        ("TAB" . corfu-next)
	("C-c" . corfu-quit)
        ([tab] . corfu-next)
        ("S-TAB" . corfu-previous)
        ([backtab] . corfu-previous))
  :hook
  (after-init . global-corfu-mode))

(use-package corfu-history
  :after (corfu)
  :hook
  (after-init . corfu-history-mode))

(use-package corfu-popupinfo
  :after corfu
  :config
  (corfu-popupinfo-mode 1))

(use-package corfu-terminal
  :unless (display-graphic-p)
  :ensure t
  :hook
  (after-init . corfu-terminal-mode))

(use-package envrc
  :defer 2
  :if (executable-find "direnv")
  :bind (:map envrc-mode-map
              ("C-c e" . envrc-command-map))
  :config (envrc-global-mode))

(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block))

(use-package winner
  :unless noninteractive
  :hook
  (after-init . winner-mode))

(use-package windmove
  :bind
  ("S-<up>" . windmove-up)
  ("S-<left>" . windmove-left)
  ("S-<right>" . windmove-right)
  ("S-<down>" . windmove-down)
  ("M-S-<up>" . windmove-swap-states-up)
  ("M-S-<left>" . windmove-swap-states-left)
  ("M-S-<right>" . windmove-swap-states-right)
  ("M-S-<down>" . windmove-swap-states-down))

(use-package window
  :unless noninteractive
  :commands (shrink-window-horizontally shrink-window enlarge-window-horizontally enlarge-window)
  :bind (("S-C-<left>" . shrink-window-horizontally)
         ("S-C-<right>" . enlarge-window-horizontally)
         ("S-C-<down>" . shrink-window)
         ("S-C-<up>" . enlarge-window)))

;; Prefer ripgrep (rg) if present (instead of grep)
(setq xref-search-program
      (cond
       ((or (executable-find "ripgrep")
            (executable-find "rg"))
        'ripgrep)
       ((executable-find "ugrep")
        'ugrep)
       (t
        'grep)))

(use-package rg
  :if (executable-find "rg")
  :commands (rg rg-project rg-dwim)
  :bind (("M-s r r" . rg)
         ("M-s r p" . rg-project)
         ("M-s r s" . rg-dwim))
  :custom
  (rg-group-result t)
  (rg-hide-command t)
  (rg-show-columns nil)
  (rg-show-header t)
  (rg-default-alias-fallback "all")
  :config
  (cl-pushnew '("tmpl" . "*.tmpl") rg-custom-type-aliases)
  (cl-pushnew '("gotest" . "*_test.go") rg-custom-type-aliases)
  (defun vde/rg-buffer-name ()
    "Generate a rg buffer name from project if in one"
    (let ((p (project-root (project-current))))
      (if p
	  (format "rg: %s" (abbreviate-file-name p))
	"rg")))
  (setq rg-buffer-name #'vde/rg-buffer-name))

(use-package wgrep
  :unless noninteractive
  :commands (wgrep-change-to-wgrep-mode)
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t)
  :bind (:map grep-mode-map
              ("e" . wgrep-change-to-wgrep-mode)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(use-package tempel
  :custom (tempel-path (expand-file-name "templates" user-emacs-directory))
  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert)))

(use-package embark
  :unless noninteractive
  :commands (embark-act embark-dwim embark-prefix-help-command)
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("C-h M" . embark-bindings-in-keymap)
  (:map completion-list-mode-map
        ("." . embark-act))
  :custom
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?"))

(use-package pr-review
  :commands (pr-review pr-review-open pr-review-submit-review)
  :custom
  (pr-review-ghub-host "api.github.com")
  (pr-review-notification-include-read nil)
  (pr-review-notification-include-unsubscribed nil))

(use-package pr-review-search
  :commands (pr-review-search pr-review-search-open pr-review-current-repository pr-review-current-repository-search)
  :config
  (defun pr-review-current-repository-search (query)
    "Run pr-review-search on the current repository."
    (interactive "sSearch query: ")
    (pr-review-search (format "is:pr archived:false is:open repo:%s %s" (vde/gh-get-current-repo) query)))
  
  (defun pr-review-current-repository ()
    "Run pr-review-search on the current repository."
    (interactive)
    (pr-review-search (format "is:pr archived:false is:open repo:%s" (vde/gh-get-current-repo)))))

(use-package jinx
  :hook (emacs-startup . global-jinx-mode)
  :bind (([remap ispell-word] . jinx-correct) ;; ("M-$" . jinx-correct)
         ("C-M-$" . jinx-languages)))

(use-package eljira
  :commands (eljira)
  :ensure nil
  :load-path "~/src/github.com/sawwheet/eljira/"
  :custom
  (eljira-token (passage-get "redhat/issues/token/myji"))
  (eljira-username "vdemeest@redhat.com")
  (eljira-url "https://issues.redhat.com"))

(use-package chatgpt-shell
  :commands (chatgpt-shell)
  :custom
  (chatgpt-shell-google-key (passage-get "ai/gemini/api_key"))
  (chatgpt-shell-openrouter-key (passage-get "ai/openroute/api_key"))
  (chatgpt-shell-deepseek-key (passage-get "ai/deepseek/api_key")))

;; TODO window management
;; TODO ORG mode configuration (BIG one)
(use-package org
  :if (file-exists-p org-directory)
  :mode (("\\.org$" . org-mode)
         ("\\.org.draft$" . org-mode))
  :commands (org-agenda org-capture)
  :bind (("C-c o l" . org-store-link)
         ("C-c o r r" . org-refile)
	 ;; ("C-c o r R" . vde/reload-org-refile-targets)
         ("C-c o a a" . org-agenda)
	 ;; ("C-c o a r" . vde/reload-org-agenda-files)
	 ;; ("C-c C-x i" . vde/org-clock-in-any-heading)
         ("C-c o s" . org-sort)
	 ("C-c O" . org-open-at-point-global)
         ("<f12>" . org-agenda))
  :custom
  (org-use-speed-commands t)
  (org-special-ctrl-a/e t)
  (org-special-ctrl-k t)
  (org-hide-emphasis-markers t)
  (org-pretty-entities t)
  (org-ellipsis "…")
  (org-return-follows-link t)
  (org-todo-keywords '((sequence "STRT(s)" "NEXT(n)" "TODO(t)" "WAIT(w)" "|" "DONE(d!)" "CANX(c@/!)")))
  (org-todo-state-tags-triggers '(("CANX" ("CANX" . t))
                                  ("WAIT" ("WAIT" . t))
                                  (done ("WAIT"))
                                  ("TODO" ("WAIT") ("CANX"))
                                  ("NEXT" ("WAIT") ("CANX"))
                                  ("DONE" ("WAIT") ("CANX"))))
  (org-tag-alist
   '((:startgroup)
     ("Handson" . ?o)
     (:grouptags)
     ("Write" . ?w) ("Code" . ?c)
     (:endgroup)
     
     (:startgroup)
     ("Handsoff" . ?f)
     (:grouptags)
     ("Read" . ?r) ("Watch" . ?W) ("Listen" . ?l)
     (:endgroup)))
  (org-log-done 'time)
  (org-log-redeadline 'time)
  (org-log-reschedule 'time)
  (org-log-into-drawer t)
  ;; https://jeffbradberry.com/posts/2025/05/orgmode-priority-cookies/
  ;; 1 2 and 3 are high, 4 is default, 5 is "hide / whenever or maybe never"
  (org-priority-highest 1)
  (org-priority-lowest  5)
  (org-priority-default 4)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+")))
  (org-agenda-file-regexp "^[a-zA-Z0-9-_]+.org$")
  (org-agenda-files `(,org-inbox-file ,org-todos-file))
  (org-agenda-remove-tags t)
  (org-agenda-span 'day)
  (org-agenda-start-on-weekday 1)
  (org-agenda-window-setup 'current-window)
  (org-agenda-sticky t)
  (org-agenda-sorting-strategy
   '((agenda time-up deadline-up scheduled-up todo-state-up priority-down)
     (todo todo-state-up priority-down deadline-up)
     (tags todo-state-up priority-down deadline-up)
     (search todo-state-up priority-down deadline-up)))
  (org-agenda-custom-commands
   '(
     ;; Archive tasks
     ("#" "To archive" todo "DONE|CANX")
     ;; TODO take inspiration from those
     ;; ("$" "Appointments" agenda* "Appointments")
     ;; ("b" "Week tasks" agenda "Scheduled tasks for this week"
     ;;  ((org-agenda-category-filter-preset '("-RDV")) ; RDV for Rendez-vous
     ;;   (org-agenda-use-time-grid nil)))
     ;; 
     ;; ;; Review started and next tasks
     ;; ("j" "STRT/NEXT" tags-todo "TODO={STRT\\|NEXT}")
     ;; 
     ;; ;; Review other non-scheduled/deadlined to-do tasks
     ;; ("k" "TODO" tags-todo "TODO={TODO}+DEADLINE=\"\"+SCHEDULED=\"\"")
     ;; 
     ;; ;; Review other non-scheduled/deadlined pending tasks
     ;; ("l" "WAIT" tags-todo "TODO={WAIT}+DEADLINE=\"\"+SCHEDULED=\"\"")
     ;; 
     ;; ;; Review upcoming deadlines for the next 60 days
     ;; ("!" "Deadlines all" agenda "Past/upcoming deadlines"
     ;;  ((org-agenda-span 1)
     ;;   (org-deadline-warning-days 60)
     ;;   (org-agenda-entry-types '(:deadline))))

     ("d" "Daily Agenda"
      ((agenda ""
	       ((org-agenda-span 'day)
		(org-deadline-warning-days 5)))
       (tags-todo "+PRIORITY=\"1\""
		  ((org-agenda-overriding-header "High Priority Tasks")))
       (todo "NEXT"
	     ((org-agenda-overriding-header "Next Tasks")))))
     ("D" "Daily Agenda (old)"
      ((agenda ""
	       ((org-agenda-files (vde/all-org-agenda-files))
		(org-agenda-span 'day)
		(org-deadline-warning-days 5)))
       (tags-todo "+PRIORITY=\"A\""
		  ((org-agenda-files (vde/all-org-agenda-files))
		   (org-agenda-overriding-header "High Priority Tasks")))
       (todo "NEXT"
	     ((org-agenda-files (vde/all-org-agenda-files))
	      (org-agenda-overriding-header "Next Tasks")))))
     ("i" "Inbox (triage)"
      ((tags-todo ".*"
		  ((org-agenda-files `(,org-inbox-file)) ;; FIXME use constant here
		   (org-agenda-overriding-header "Unprocessed Inbox Item")))))
     ("A" "All (old)"
      ((tags-todo ".*"
		  ((org-agenda-files (vde/all-org-agenda-files))))))
     ("u" "Untagged Tasks"
      ((tags-todo "-{.*}"
		  ((org-agenda-overriding-header "Untagged tasks")))))
     ("w" "Weekly Review"
      ((agenda ""
	       ((org-agenda-overriding-header "Completed Tasks")
		(org-agenda-skip-function '(org-agenda-skip-entry-if 'nottodo 'done))
		(org-agenda-span 'week)))
       (agenda ""
	       ((org-agenda-overriding-header "Unfinished Scheduled Tasks")
		(org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
		(org-agenda-span 'week)))))
     ;; FIXME Should only take into account projects and areas ?
     ("R" "Review projects" tags-todo "-CANX/"
      ((org-agenda-overriding-header "Reviews Scheduled")
       (org-agenda-skip-function 'org-review-agenda-skip)
       (org-agenda-cmp-user-defined 'org-review-compare)
       (org-agenda-sorting-strategy '(user-defined-down))))))
  ;; TODO cleanup this list a bit
  (org-agenda-category-icon-alist `(("personal"  ,(list (propertize "🏡")))
				    ("work"  ,(list (propertize "🏢")))
				    ("appointments"  ,(list (propertize "📅")))
				    ("health"  ,(list (propertize "⚕️")))
				    ("systems"  ,(list (propertize "🖥️")))
				    ("journal"  ,(list (propertize "📝")))
				    ("project--" ,(list (propertize "💼" )))
				    ("tekton", (list (propertize "😼")))
				    ("openshift-pipelines", (list (propertize "🎩")))
				    ("redhat", (list (propertize "🎩")))
				    ("area--"  ,(list (propertize"🏢" )))
				    ("area--home"  ,(list (propertize "🏡")))
				    ("home"  ,(list (propertize "🏡")))
				    ("home-services" ,(list (propertize "☕ ")))
				    ("email"  ,(list (propertize"📨" )))
				    ("people"  ,(list (propertize"👤" )))
				    ("machine" ,(list (propertize "🖥️")))
				    ("website" ,(list (propertize "🌍")))
				    ("bike" ,(list (propertize "🚴‍♂️")))
				    ("security" ,(list (propertize "🛡️")))
				    ("i*" ,(list (propertize "📒")))))
  (org-agenda-prefix-format '((agenda . " %i %?-12t% s")
			      (todo . " %i")
			      (tags . " %i")
			      (search . " %i")))
  (org-insert-heading-respect-content t)
  (org-M-RET-may-split-line '((default . nil)))
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)
  (org-goto-max-level 2)
  :bind
  (:map org-mode-map
	("C-<left>" . org-shiftleft)
	("C-<right>" . org-shiftright)
	("C-<up>" . org-shiftup)
	("C-<down>" . org-shiftdown))
  :config
  (unbind-key "S-<left>" org-mode-map)
  (unbind-key "S-<right>" org-mode-map)
  (unbind-key "S-<up>" org-mode-map)
  (unbind-key "S-<down>" org-mode-map)
  (unbind-key "M-S-<left>" org-mode-map)
  (unbind-key "M-S-<right>" org-mode-map)
  (unbind-key "M-S-<up>" org-mode-map)
  (unbind-key "M-S-<down>" org-mode-map)
  (unbind-key "C-S-<left>" org-mode-map)
  (unbind-key "C-S-<right>" org-mode-map)
  (unbind-key "C-S-<up>" org-mode-map)
  (unbind-key "C-S-<down>" org-mode-map))

(use-package org-agenda
  :after org
  :commands (org-agenda)
  :config
  (unbind-key "S-<left>" org-agenda-mode-map)
  (unbind-key "S-<right>" org-agenda-mode-map)
  (unbind-key "S-<up>" org-agenda-mode-map)
  (unbind-key "S-<down>" org-agenda-mode-map)
  (unbind-key "C-S-<left>" org-agenda-mode-map)
  (unbind-key "C-S-<right>" org-agenda-mode-map))

;; Make sure we load org-protocol
(use-package org-protocol
  :after org)

(use-package org-tempo
  :after (org)
  :custom
  (org-structure-template-alist '(("a" . "aside")
				  ("c" . "center")
				  ("C" . "comment")
				  ("e" . "example")
				  ("E" . "export")
				  ("Ea" . "export ascii")
				  ("Eh" . "export html")
				  ("El" . "export latex")
				  ("q" . "quote")
				  ("s" . "src")
				  ("se" . "src emacs-lisp")
				  ("sE" . "src emacs-lisp :results value code :lexical t")
				  ("sg" . "src go")
				  ("sr" . "src rust")
				  ("sp" . "src python")
				  ("v" . "verse"))))

(use-package org-capture
  :after org
  :commands (org-capture)
  :config

  (add-to-list 'org-capture-templates
	       `("j" "Journal entry" item
		 (file+datetree+prompt ,org-journal-file)
		 "%U %?\n%i"))
  
  ;; TODO: refine this, create a function that reset this
  (add-to-list 'org-capture-templates
               `("l" "Link" entry
                 (file ,org-inbox-file)
                 "* %a\n%U\n%?\n%i"
                 :empty-lines 1))
  (add-to-list 'org-capture-templates
               `("t" "Tasks"))
  (add-to-list 'org-capture-templates
               `("tt" "New task" entry
                 (file ,org-inbox-file)
                 "* %?\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n\n%i\n\nFrom: %a"
                 :empty-lines 1))
  ;; Refine this
  (add-to-list 'org-capture-templates
               `("tr" "PR Review" entry
                 (file ,org-inbox-file)
                 "* TODO review gh:%^{issue} :review:\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n%?\nFrom: %a"
                 :empty-lines 1))
  ;; emails
  (add-to-list 'org-capture-templates
	       `("m" "Email Workflow"))
  (add-to-list 'org-capture-templates
	       `("mf" "Follow Up" entry
		 (file ,org-inbox-file)
		 "* TODO Follow up with %:from on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i"
		 :immediate-finish t))
  (add-to-list 'org-capture-templates
	       `("mr" "Read Later" entry
		 (file ,org-inbox-file)
		 "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t))
  ;; (add-to-list 'org-capture-templates
  ;;              `("m" "Meeting notes" entry
  ;;                (file+datetree ,org-meeting-notes-file)
  ;;                (file ,(concat user-emacs-directory "/etc/orgmode/meeting-notes.org"))))
  
  (defun vde/window-delete-popup-frame (&rest _)
    "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
    (when (frame-parameter nil 'vde/window-popup-frame)
      (delete-frame)))

  (add-to-list 'org-capture-templates
               `("w" "Writing"))
  (add-hook 'org-capture-after-finalize-hook #'vde/window-delete-popup-frame)
  :bind (("C-c o c" . org-capture)))

(use-package org-habit
  :after org
  :custom
  (org-habit-show-habits-only-for-today nil)
  (org-habit-graph-column 80))

(use-package denote
  :commands (denote)
  :bind (("C-c n c" . denote-region)
	 ("C-c n i" . denote-link-or-create)
	 ("C-c n b" . denote-backlinks)
	 ("C-c n F f" . denote-find-link)
	 ("C-c n F b" . denote-find-backlink))
  :custom
  (denote-directory org-notes-directory)
  (denote-rename-buffer-format "📝 %t")
  (denote-date-prompt-denote-date-prompt-use-org-read-date t)
  (denote-prompts '(title keywords))
  (denote-backlinks-display-buffer-action
   '((display-buffer-reuse-window
      display-buffer-in-side-window)
     (side . bottom)
     (slot . 99)
     (window-width . 0.3)
     (dedicated . t)
     (preserve-size . (t . t))))
  :hook (dired-mode . denote-dired-mode)
  :config
  (denote-rename-buffer-mode 1)
  (defun my-denote-always-rename-on-save-based-on-front-matter ()
    "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields.

Add this function to the `after-save-hook'."
    (let ((denote-rename-confirmations nil)
          (denote-save-buffers t)) ; to save again post-rename
      (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
	(ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
	(message "Buffer saved; Denote file renamed"))))

  (add-hook 'after-save-hook #'my-denote-always-rename-on-save-based-on-front-matter)
  
  (defun vde/org-category-from-buffer ()
    "Get the org category (#+category:) value from the buffer"
    (cond
     ((string-match "__journal.org$" (buffer-file-name))
      "journal")
     (t
      (denote-sluggify (denote--retrieve-title-or-filename (buffer-file-name) 'org))))))

(use-package denote-org
  :after (denote org)
  :defer 2)

;; (use-package whisper
;;   :commands (whisper-run whisper-file)
;;   :custom
;;   (whisper-install-whispercpp nil))
;; TODO gptel configuration (and *maybe* copilot)

(provide 'init)
;;; init.el ends here
