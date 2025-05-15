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
  (setopt modus-themes-to-rotate '(modus-operandi modus-vivendi)
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

(use-package emacs
  :bind
  ("C-x m" . mark-defun)
  :custom
  (enable-local-variables :all)
  (select-enable-clipboard t)
  (select-enable-primary t)
  (comment-multi-line t)
  (make-backup-files nil)
  :hook
  (after-init . global-hl-line-mode)
  (after-init . global-completion-preview-mode)
  :config
  (delete-selection-mode 1))

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

(use-package minions
  :hook (after-init . minions-mode)
  :config
  (add-to-list 'minions-prominent-modes 'flymake-mode))

# TODO ORG mode configuration (BIG one)
