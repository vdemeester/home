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
(defconst org-habits-file (expand-file-name "habits.org" org-directory)
  "`org-mode' file for habits.  This is the file for habits that I need to share elsewhere, with Flat habits on iOS for example.")
(defconst org-journal-file (expand-file-name "20250620T144103--journal__journal.org" org-notes-directory)
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
  (setopt modus-themes-common-palette-overrides
	  `((border-mode-line-active unspecified)
            (border-mode-line-inactive unspecified)
	    ,@modus-themes-preset-overrides-cooler)
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
;; (setopt debug-on-error t)
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

(require 'init-func)
;; TODO: do useful stuff with the macro instead
(vde/run-and-delete-frame my-greet-and-close ()
			  "Displays a greeting and closes the frame after a short delay."
			  (message "Hello from a macro-defined function! Closing soon...")
			  (sit-for 2))

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
  (global-auto-revert-non-file-buffers t "Auto revert non-file buffers")
  (switch-to-buffer-obey-display-actions t "Don't distuingish automatic and manual window switching")
  (window-combination-resize t "Resize window proportionally")
  (isearch-lazy-count t "Show size of search results")
  (lazy-count-prefix-format "(%s/%s) " "Format of search results")
  (lazy-highlight-initial-delay 0 "No delay before highlight search matches")
  (isearch-allow-scroll t "Allow scrolling while searching")
  (isearch-allow-motion t "Allow movement commands while searching")
  :hook
  (after-init . global-hl-line-mode)
  (after-init . global-completion-preview-mode)
  (after-init . auto-insert-mode)
  (after-init . pixel-scroll-mode)
  :config
  (with-current-buffer "*Messages*" (emacs-lock-mode 'kill))
  (with-current-buffer "*scratch*" (emacs-lock-mode 'kill))
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

(use-package tramp
  :custom
  ;; Tramp
  (remote-file-name-inhibit-locks t "No lock files on remote files")
  (tramp-use-scp-direct-remote-copying t "Use direct copying between two remote hosts")
  (remote-file-name-inhibit-auto-save-visited t "Do not auto-save remote files")
  (tramp-copy-size-limit (* 1024 1024)) ;; 1MB
  (tramp-verbose 2)
  :config
  (connection-local-set-profile-variables
   'remote-direct-async-process
   '((tramp-direct-async-process . t)))

  (connection-local-set-profiles
   '(:application tramp :protocol "scp")
   'remote-direct-async-process)

  (setq magit-tramp-pipe-stty-settings 'pty)
  (with-eval-after-load 'tramp
    (with-eval-after-load 'compile
      (remove-hook 'compilation-mode-hook #'tramp-compile-disable-ssh-controlmaster-options)))
  
  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)
  (add-to-list 'tramp-remote-path "/home/vincent/.local/state/nix/profile/bin/")
  (add-to-list 'tramp-remote-path "~/bin/")
  ;; (add-to-list 'tramp-connection-properties
  ;; 	       (list (regexp-quote "/ssh:aomi.home:")
  ;; 		     "remote-shell" "/home/vincent/.local/state/nix/profile/bin/zsh"))
  )

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
  (:map flymake-mode-map
        ("M-n" . flymake-goto-next-error)
        ("M-p" . flymake-goto-prev-error))
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
  ;; (dired-mode . dired-sort-toggle-or-edit) ; I don't like the "default by date" behavior
  )

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
  (add-to-list 'eglot-server-programs
	       '(go-mode . ("harper-ls" "--stdio")))
  ;; (add-to-list 'eglot-server-programs
  ;; 	       '(text-mode . ("harper-ls" "--stdio")))
  ;; (add-to-list 'eglot-server-programs
  ;;              '(org-mode . ("harper-ls" "--stdio")))
  (add-to-list 'eglot-server-programs
               '(markdown-mode . ("harper-ls" "--stdio")))
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
  (sh-script-mode . eglot-ensure)
  (org-mode . eglot-ensure)
  (markdown-mode . eglot-ensure))

(setq major-mode-remap-alist
      '((python-mode . python-ts-mode)
	(go-mode . go-ts-mode)))

(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :hook ((markdown-mode . visual-line-mode)
	 (gfm-mode . visual-line-mode)))

(use-package yaml-ts-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-ts-mode . display-line-numbers-mode)
	 (yaml-ts-mode . outline-minor-mode)
	 (yaml-ts-mode . electric-pair-local-mode))
  :config
  (setq-local outline-regexp "^ *\\([A-Za-z0-9_-]*: *[>|]?$\\|-\\b\\)")
  (font-lock-add-keywords
   'yaml-ts-mode
   '(("\\($(\\(workspaces\\|context\\|params\\)\.[^)]+)\\)" 1 'font-lock-constant-face prepend)
     ("kind:\s*\\(.*\\)\n" 1 'font-lock-keyword-face prepend))))

(use-package orgalist
  :commands (orgalist-mode)
  :hook ((markdown-mode . orgalist-mode)
	 (gfm-mode . orgalist-mode)))

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
  (magit-refresh-status-buffer nil "don't automatically refresh the status buffer after running a git command")
  (magit-commit-show-diff nil "don't show the diff by default in the commit buffer. Use `C-c C-d' to display it")
  (magit-branch-direct-configure nil "don't show git variables in magit branch")
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
  :custom
  (envrc-remote t)
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
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")
  :config
  (keymap-set embark-symbol-map "S i" #'symbol-overlay-put)
  (keymap-set embark-symbol-map "S c" #'symbol-overlay-remove-all)
  (keymap-set embark-symbol-map "S r" #'symbol-overlay-rename)
  (defun vde/short-github-link ()
    "Target a link at point of the for github:owner/repo#number"
    )
  )

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
  (org-agenda-files `(,org-inbox-file ,org-todos-file ,org-habits-file))
  (org-refile-targets '((org-agenda-files :maxlevel . 3)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
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
  :hook
  (org-mode . auto-fill-mode)
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
  (unbind-key "C-S-<down>" org-mode-map)

  (require 'dash)
  (require 's)
  (defun vde/org-refile-targets ()
    (append '((org-inbox-file :level . 0)
	      (org-todos-file :maxlevel . 3))
	    (->>
	     (directory-files org-notes-directory nil ".org$")
	     (--remove (s-starts-with? "." it))
	     (--remove (s-contains? "==readwise=" it))
	     (--map (format "%s/%s" org-notes-directory it))
	     (--map `(,it :maxlevel . 3)))
	    (->>
	     (directory-files org-people-dir ".org$")
	     (--remove (s-starts-with? (format "%s/legacy" org-people-dir) it))
	     (--map (format "%s" it))
	     (--map `(,it :maxlevel . 3))))))

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
  :commands (org-capture)
  :bind (("C-c o c" . org-capture))
  :config
  (add-to-list 'org-capture-templates
	       `("j" "🗞 Journal entry" item
		 (file+datetree ,org-journal-file)
		 "%U %?\n%i")
	       t)
  (add-to-list 'org-capture-templates
	       `("t" "📥 Tasks")
	       t)
  (add-to-list 'org-capture-templates
	       `("tt" " New task" entry
		 (file ,org-inbox-file)
		 "* TODO %?\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n\n%i\n\nFrom: %a"
		 :empty-lines 1)
	       t)
  (add-to-list 'org-capture-templates
	       `("tl" " New task (from capture)" entry
		 (file ,org-inbox-file)
		 "* TODO %a\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n\n%i\n\nFrom: %a"
		 :empty-lines 1)
	       t)
  (add-to-list 'org-capture-templates
	       `("td" "✅ Done" entry
		 (file ,org-inbox-file)
		 "* DONE %?\n:PROPERTIES:\n:CREATED:\t%U\n:END:\n\n%i\n\nFrom: %a"
		 :empty-lines 1)
	       t)
  ;; Refine this
  (add-to-list 'org-capture-templates
	       `("tr" " PR Review" entry
		 (file ,org-inbox-file)
		 "* TODO review gh:%^{issue} :review:\n:PROPERTIES:\n:CREATED:%U\n:END:\n\n%i\n%?\nFrom: %a"
		 :empty-lines 1)
	       t)
  (add-to-list 'org-capture-templates
	       `("l" "🔗 Link" entry
		 (file ,org-inbox-file)
		 "* %a\n%U\n%?\n%i"
		 :empty-lines 1)
	       t)
  (add-to-list 'org-capture-templates
	       `("m" "✉ Email Workflow")
	       t)
  (add-to-list 'org-capture-templates
	       `("mf" "Follow Up" entry
		 (file ,org-inbox-file)
		 "* TODO Follow up with %:from on %a\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%i"
		 :immediate-finish t)
	       t)
  (add-to-list 'org-capture-templates
	       `("mr" "Read Later" entry
		 (file ,org-inbox-file)
		 "* TODO Read %:subject\nSCHEDULED:%t\nDEADLINE: %(org-insert-time-stamp (org-read-date nil t \"+2d\"))\n\n%a\n\n%i" :immediate-finish t)	       
	       t)
  (add-to-list 'org-capture-templates
	       `("J" "🗞 Journal (antidated) entry" item
		 (file+datetree+prompt ,org-journal-file)
		 "%U %?\n%i")
	       t)
  ;; TODO: refine this, create a function that reset this
  ;; emails
  ;; (add-to-list 'org-capture-templates
  ;;              `("m" "Meeting notes" entry
  ;;                (file+datetree ,org-meeting-notes-file)
  ;;                (file ,(concat user-emacs-directory "/etc/orgmode/meeting-notes.org"))))
  
  (defun vde/window-delete-popup-frame (&rest _)
    "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
    (when (frame-parameter nil 'vde/window-popup-frame)
      (delete-frame)))

  ;; (add-to-list 'org-capture-templates
  ;; `("w" "Writing"))
  (add-hook 'org-capture-after-finalize-hook #'vde/window-delete-popup-frame))

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

(use-package mu4e
  :commands (mu4e)
  :custom
  (mu4e-mu-home "/home/vincent/.local/cache/mu")
  (mu4e-context-policy 'pick-first)
  (mu4e-change-filenames-when-moving t)
  (mu4e-attachment-dir "~/desktop/downloads")
  :config
  (setq mu4e-get-mail-command (concat (executable-find "mbsync") " --all"))
  (setq mu4e-update-interval 1800) ; 30m
  
  (defun vde-mu4e--mark-get-copy-target ()
    "Ask for a copy target, and propose to create it if it does not exist."
    (let* ((target (mu4e-ask-maildir "Copy message to: "))
           (target (if (string= (substring target 0 1) "/")
                       target
                     (concat "/" target)))
           (fulltarget (mu4e-join-paths (mu4e-root-maildir) target)))
      (when (mu4e-create-maildir-maybe fulltarget)
	target)))

  (defun copy-message-to-target(docid msg target)
    (let (
          (new_msg_path nil) ;; local variable                                                                
          (msg_flags (mu4e-message-field msg :flags))                                                                                                       
          )                                                                                                   
      ;; 1. target is already determined interactively when executing the mark (:ask-target)                     

      ;; 2. Determine the path for the new file: we use mu4e~draft-message-filename-construct from            
      ;; mu4e-draft.el to create a new random filename, and append the original's msg_flags                   
      (setq new_msg_path (format "%s/%s/cur/%s" mu4e-maildir target (mu4e~draft-message-filename-construct    
								     (mu4e-flags-to-string msg_flags))))                                                                     

      ;; 3. Copy the message using file system call (copy-file) to new_msg_path:                              
      ;; (See e.g. mu4e-draft.el > mu4e-draft-open > resend)                                             
      (copy-file (mu4e-message-field msg :path) new_msg_path)                                                 

      ;; 4. Add the information to the database (may need to update current search query with 'g' if duplicating to current box. Try also 'V' to toggle the display of duplicates) 
      (mu4e~proc-add new_msg_path (mu4e~mark-check-target target))                                              
      )                                                                                                       
    )

  (defun vde-mu4e--refile (msg)
    "Refile function to smartly move `MSG' to a given folder."
    (cond
     ;; FIXME
     ((string= (plist-get (car-safe (mu4e-message-field msg :cc)) :email) "ci_activity@noreply.github.com")
      "/icloud/Deleted Messages")
     (t
      (let ((year (format-time-string "%Y" (mu4e-message-field msg :date))))
	(format "/icloud/Archives/%s" year)))))

  (setq
   mu4e-headers-draft-mark     '("D" . "💈")
   mu4e-headers-flagged-mark   '("F" . "📍")
   mu4e-headers-new-mark       '("N" . "🔥")
   mu4e-headers-passed-mark    '("P" . "❯")
   mu4e-headers-replied-mark   '("R" . "❮")
   mu4e-headers-seen-mark      '("S" . "☑")
   mu4e-headers-trashed-mark   '("T" . "💀")
   mu4e-headers-attach-mark    '("a" . "📎")
   mu4e-headers-encrypted-mark '("x" . "🔒")
   mu4e-headers-signed-mark    '("s" . "🔑")
   mu4e-headers-unread-mark    '("u" . "⎕")
   mu4e-headers-list-mark      '("l" . "🔈")
   mu4e-headers-personal-mark  '("p" . "👨")
   mu4e-headers-calendar-mark  '("c" . "📅"))

  (setopt mu4e-completing-read-function completing-read-function)
  (setq mu4e-refile-folder 'vde-mu4e--refile)
  (setq mu4e-contexts `( ,(make-mu4e-context
			   :name "icloud"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/icloud" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (user-mail-address . "vincent@demeester.fr")
				   (mu4e-trash-folder . "/icloud/Deleted Messages")
				   (mu4e-sent-folder . "/icloud/Sent Messages")
				   (mu4e-draft-folder . "/icloud/Drafts")
				   ;; (mu4e-get-mail-command . "mbsync icloud")
				   ))
			 ,(make-mu4e-context
			   :name "gmail"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/gmail" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (user-mail-address . "vinc.demeester@gmail.com")
				   (mu4e-drafts-folder  . "/gmail/[Gmail]/Drafts")
				   (mu4e-sent-folder  . "/gmail/[Gmail]/Sent Mail")
				   ;; (mu4e-refile-folder  . "/gmail/[Gmail]/All Mail")
				   (mu4e-trash-folder  . "/gmail/[Gmail]/Trash")
				   ;; (mu4e-get-mail-command . "mbsync gmail")
				   ))
			 ,(make-mu4e-context
			   :name "redhat"
			   :match-func (lambda (msg) (when msg
						       (string-prefix-p "/redhat" (mu4e-message-field msg :maildir))))
			   :vars '(
				   (user-mail-address . "vdemeest@redhat.com")
				   (mu4e-drafts-folder  . "/redhat/[Gmail]/Drafts")
				   (mu4e-sent-folder  . "/redhat/[Gmail]/Sent Mail")
				   ;; (mu4e-refile-folder  . "/redhat/[Gmail]/All Mail")
				   (mu4e-trash-folder  . "/redhat/[Gmail]/Trash")
				   ;; (mu4e-get-mail-command . "mbsync redhat")
				   ))
			 ))
  (add-to-list 'mu4e-bookmarks
	       '( :name  "All Inboxes"
		  :query "maildir:/icloud/INBOX OR maildir:/gmail/INBOX OR maildir:/redhat/INBOX"
		  :key   ?b))
  (with-eval-after-load "mm-decode"
    (add-to-list 'mm-discouraged-alternatives "text/html")
    (add-to-list 'mm-discouraged-alternatives "text/richtext")))

;; (use-package whisper
;;   :commands (whisper-run whisper-file)
;;   :custom
;;   (whisper-install-whispercpp nil))
;; TODO gptel configuration (and *maybe* copilot)

(use-package goose
  :commands (goose-transient goose-start-session)
  :bind (("C-c a G" . goose-transient)))

(use-package mcp
  :commands (mcp-hub-start-all-server)
  :after gptel
  :custom (mcp-hub-servers
	   `(("jira"
	      :command "/home/vincent/src/github.com/chmouel/jayrah/.venv/bin/jayrah"
	      :args ("mcp"))
	     ("github"
	      :command "github-mcp-server"
	      :args ("stdio")
	      :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(passage-get "github/vdemeester/github-mcp-server")))
	     ("playwright"
	      :command "npx @playwright/mcp@latest"
	      :args ("--executable-path", "/run/current-system/sw/bin/chromium"))))
  :config (require 'mcp-hub))

(use-package gptel
  :commands (gptel gptel-mode)
  :bind (("C-c a g" . gptel))
  :hook
  (gptel-mode . visual-line-mode)
  :bind
  (:map gptel-mode-map
        ("C-c C-k" . gptel-abort)
        ("C-c C-m" . gptel-menu)
        ("C-c C-c" . gptel-send))
  :custom
  (gptel-default-mode #'markdown-mode)
  :config
  (require 'gptel-curl)
  (require 'gptel-gemini)
  (require 'gptel-ollama)
  (require 'gptel-transient)
  (require 'gptel-integrations)
  (require 'gptel-rewrite)
  (require 'gptel-org)
  (require 'gptel-openai)
  (require 'gptel-openai-extras)
  (require 'gptel-autoloads)
  (gptel-mcp-connect)
  
  (setq gptel-model 'gemini-2.5-flash
	gptel-backend (gptel-make-gemini "Gemini"
			:key (passage-get "ai/gemini/api_key"))
	)

  (gptel-make-gemini "Gemini Red Hat"
    :key (passage-get "redhat/google/osp/vdeemest-api-key"))
  
  (gptel-make-openai "MistralLeChat"
    :host "api.mistral.ai/v1"
    :endpoint "/chat/completions"
    :protocol "https"
    :key (passage-get "ai/mistralai/api_key")
    :models '("mistral-small"))
  
  (gptel-make-openai "OpenRouter"
    :host "openrouter.ai"
    :endpoint "/api/v1/chat/completions"
    :stream t
    :key (passage-get "ai/openroute/api_key")
    :models '(cognittivecomputations/dolphin3.0-mistral-24b:free
	      cognitivecomputations/dolphin3.0-r1-mistral-24b:free
	      deepseek/deepseek-r1-zero:free
	      deepseek/deepseek-chat:free
	      deepseek/deepseek-r1-distill-qwen-32b:free
	      deepseek/deepseek-r1-distill-llama-70b:free
	      google/gemini-2.0-flash-lite-preview-02-05:free
	      google/gemini-2.0-pro-exp-02-05:free
	      google/gemini-2.5-pro-exp-03-25:free
	      google/gemma-3-12b-it:free
	      google/gemma-3-27b-it:free
	      google/gemma-3-4b-it:free
	      mistralai/mistral-small-3.1-24b-instruct:free
	      open-r1/olympiccoder-32b:free
	      qwen/qwen2.5-vl-3b-instruct:free
	      qwen/qwen-2.5-coder-32b-instruct:free
	      qwen/qwq-32b:free
	      codellama/codellama-70b-instruct
	      google/gemini-pro
	      google/palm-2-codechat-bison-32k
	      meta-llama/codellama-34b-instruct
	      mistralai/mixtral-8x7b-instruct
	      openai/gpt-3.5-turbo))

  ;; TODO: configure shikoku/kobe ollama instances here
  ;; (gptel-make-ollama "Ollama"
  ;;   :host "localhost:11434"
  ;;   :stream t
  ;;   :models '("smollm:latest"
  ;; 	      "llama3.1:latest"
  ;; 	      "deepseek-r1:latest"
  ;; 	      "mistral-small:latest"
  ;; 	      "deepseek-r1:7b"
  ;; 	      "nomic-embed-text:latest"))
  )

(use-package devdocs
  :commands (devdocs-lookup devdocs-install vde/install-devdocs)
  :bind (("C-h D" . devdocs-lookup))
  :config
  (defun vde/install-devdocs ()
    "Install the devdocs I am using the most."
    (interactive)
    (dolist (docset '("bash"
		      "c"
		      "click"
		      "cpp"
		      "css"
		      "elisp"
		      "flask"
		      "git"
		      "gnu_make"
		      "go"
		      "html"
		      "htmx"
		      "http"
		      "javascript"
		      "jq"
		      "jquery"
		      "kubectl"
		      "kubernetes"
		      "lua~5.4"
		      "nix"
		      "python~3.13"
		      "python~3.12"
		      "requests"
		      "sqlite"
		      "terraform"
		      "werkzeug"
		      "zig"))
      (devdocs-install docset))))

(defvar highlight-codetags-keywords
  '(("\\<\\(TODO\\|FIXME\\|BUG\\|XXX\\)\\>" 1 font-lock-warning-face prepend)
    ("\\<\\(NOTE\\|HACK\\)\\>" 1 font-lock-doc-face prepend)))

(define-minor-mode highlight-codetags-local-mode
  "Highlight codetags like TODO, FIXME..."
  :global nil
  (if highlight-codetags-local-mode
      (font-lock-add-keywords nil highlight-codetags-keywords)
    (font-lock-remove-keywords nil highlight-codetags-keywords))

  ;; Fontify the current buffer
  (when (bound-and-true-p font-lock-mode)
    (if (fboundp 'font-lock-flush)
        (font-lock-flush)
      (with-no-warnings (font-lock-fontify-buffer)))))

(add-hook 'prog-mode-hook #'highlight-codetags-local-mode)

(defun vde/wtype-text (text)
  "Process TEXT for wtype, handling newlines properly."
  (let* ((has-final-newline (string-match-p "\n$" text))
         (lines (split-string text "\n"))
         (last-idx (1- (length lines))))
    (string-join
     (cl-loop for line in lines
              for i from 0
              collect (cond
                       ;; Last line without final newline
                       ((and (= i last-idx) (not has-final-newline))
                        (format "wtype -s 50 \"%s\"" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))
                       ;; Any other line
                       (t
                        (format "wtype -s 50 \"%s\" && wtype -k Return" 
                                (replace-regexp-in-string "\"" "\\\\\"" line)))))
     " && ")))

(define-minor-mode vde/type-mode
  "Minor mode for inserting text via wtype."
  :keymap `((,(kbd "C-c C-c") . ,(lambda () (interactive)
                                   (call-process-shell-command
                                    (vde/wtype-text (buffer-string))
                                    nil 0)
                                   (delete-frame)))
            (,(kbd "C-c C-k") . ,(lambda () (interactive)
                                   (kill-buffer (current-buffer))))))

(defun vde/type ()
  "Launch a temporary frame with a clean buffer for typing."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-float")
                             (fullscreen . 0)
                             (undecorated . t)
                             (width . 70)
                             (height . 20))))
        (buf (get-buffer-create "emacs-float")))
    (select-frame frame)
    (switch-to-buffer buf)
    (with-current-buffer buf
      (erase-buffer)
      ;; (org-mode)
      (markdown-mode) ;; more common ?
      (flyspell-mode)
      (vde/type-mode)
      (setq-local header-line-format
                  (format " %s to insert text or %s to cancel."
                          (propertize "C-c C-c" 'face 'help-key-binding)
			  (propertize "C-c C-k" 'face 'help-key-binding)))
      ;; Make the frame more temporary-like
      (set-frame-parameter frame 'delete-before-kill-buffer t)
      (set-window-dedicated-p (selected-window) t))))

(defun vde/agenda ()
  "Launch a frame with the org-agenda and the `org-todos-file' buffer."
  (interactive)
  (let ((frame (make-frame '((name . "emacs-org-agenda")))))
    (select-frame frame)
    (org-agenda nil "d")
    (split-window-horizontally)
    (other-window 1)
    (find-file org-todos-file)))

(defun memoize-remote (key cache orig-fn &rest args)
  "Memoize a value if the key is a remote path."
  (if (and key
           (file-remote-p key))
      (if-let ((current (assoc key (symbol-value cache))))
          (cdr current)
        (let ((current (apply orig-fn args)))
          (set cache (cons (cons key current) (symbol-value cache)))
          current))
    (apply orig-fn args)))
;; Memoize current project
(defvar project-current-cache nil)
(defun memoize-project-current (orig &optional prompt directory)
  (memoize-remote (or directory
                      project-current-directory-override
                      default-directory)
                  'project-current-cache orig prompt directory))

(advice-add 'project-current :around #'memoize-project-current)

;; Memoize magit top level
(defvar magit-toplevel-cache nil)
(defun memoize-magit-toplevel (orig &optional directory)
  (memoize-remote (or directory default-directory)
                  'magit-toplevel-cache orig directory))
(advice-add 'magit-toplevel :around #'memoize-magit-toplevel)

;; memoize vc-git-root
(defvar vc-git-root-cache nil)
(defun memoize-vc-git-root (orig file)
  (let ((value (memoize-remote (file-name-directory file) 'vc-git-root-cache orig file)))
    ;; sometimes vc-git-root returns nil even when there is a root there
    (when (null (cdr (car vc-git-root-cache)))
      (setq vc-git-root-cache (cdr vc-git-root-cache)))
    value))
(advice-add 'vc-git-root :around #'memoize-vc-git-root)

(provide 'init)
;;; init.el ends here
