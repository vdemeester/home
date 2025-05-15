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
  :hook
  (after-init . global-hl-line-mode)
  (after-init . global-completion-preview-mode))

(use-package icomplete
  :unless noninteractive
  :hook
  ;; (icomplete-minibuffer-setup
  ;;  . (lambda()(interactive) 
  ;;      (setq-local completion-styles '(flex partial-completion initials basic))))
  (after-init . fido-vertical-mode)
  :custom
  (icomplete-compute-delay 0.01))

(use-package orderless
  :unless noninteractive
  :config
  (setq completion-styles
	'(orderless basic substring initials flex partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-category-overrides nil)
  )
