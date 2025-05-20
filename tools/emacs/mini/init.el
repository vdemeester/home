;;; init --- vdemeester's emacs configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vincent Demeester
;; Author: Vincent Demeester <vincent@sbr.pm>

;; This file is NOT part of GNU Emacs.
;;; Commentary:
;; This is the "mini" version for now, but aims to become the default one.
;;; Code:


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
  (load-theme 'modus-operandi :no-confirm))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

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

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))

(use-package emacs
  :bind
  ("C-x m" . mark-defun)
  ;; (:map completion-preview-active-mode-map
  ;; ("M-n" . #'completion-preview-next-candidate)
  ;; ("M-p" . #'completion-preview-prev-candidate))
  :custom
  (enable-local-variables :all)
  (select-enable-clipboard t)
  (select-enable-primary t)
  (comment-multi-line t)
  (make-backup-files nil)
  (read-extended-command-predicate #'command-completion-default-include-p)
  :hook
  (after-init . global-hl-line-mode)
  (after-init . global-completion-preview-mode)
  :config
  (delete-selection-mode 1))

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
  (which-key-add-column-padding 1)
  (which-key-max-description-length 40)
  :hook
  (after-init . which-key-mode)
  :config
  
  ;; Define custom, concise descriptions for `tab-bar` commands under "C-x t"
  (which-key-add-key-based-replacements
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
    "C-x t ^ f" "Detach tab window"))

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
		'(:gopls (:usePlaceholders t)))
  
  :hook
  ;; (before-save . gofmt-before-save)
  (before-save . eglot-format-buffer)
  (nix-mode . eglot-ensure)
  (nix-ts-mode . eglot-ensure)
  (rust-mode . eglot-ensure)
  (rust-ts-mode . eglot-ensure)
  (python-mode . eglot-ensure)
  ;; (json-mode . eglot-ensure)
  ;; (yaml-mode . eglot-ensure)
  ;; (c-mode . eglot-ensure)
  ;; (cc-mode . eglot-ensure)
  (go-mode . eglot-ensure)
  (go-ts-mode . eglot-ensure)
  ;; (js-mode . eglot-ensure)
  ;; (js2-mode . eglot-ensure)
  ;; (typescript-mode . eglot-ensure)
  ;; (typescript-ts-mode . eglot-ensure)
  (sh-mode . eglot-ensure)
  (sh-script-mode . eglot-ensure))

(use-package markdown-mode
  :mode "\\.md\\'")

(use-package yaml-mode
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

(use-package project-func
  :commands (vde/project-magit-status vde/project-eat vde/project-vterm vde/project-run-in-vterm vde/project-try-local vde/open-readme))

(use-package project
  :commands (project-find-file project-find-regexp)
  :custom ((project-switch-commands '((?f "File" project-find-file)
				      (?g "Grep" project-find-regexp)
				      (?d "Dired" project-dired)
				      (?b "Buffer" project-switch-to-buffer)
				      (?q "Query replace" project-query-replace-regexp)
				      (?m "Magit" vde-project/magit-status)
				      (?e "Eshell" project-eshell)
				      (?E "Eat" vde/project-eat)
				      (?s "Vterm" vde/project-vterm)
				      (?R "README" vde/open-readme)
				      (?g "Checkout GitHub PR" checkout-github-pr)))
	   (project-mode-line t))
  :bind
  ("C-x p v" . vde-project/magit-status))

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

;; TODO embark
;; TODO window managementt
;; TODO ORG mode configuration (BIG one)

(provide 'init)
;;; init.el ends here
