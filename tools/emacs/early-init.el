;;; early-init.el -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)
(setq frame-inhibit-implied-resize t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)

(setq gc-cons-threshold 402653184
      gc-cons-percentage 0.6)

(defvar file-name-handler-alist-original file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar contrib/after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defun contrib/run-after-load-theme-hook (&rest _)
  "Run `contrib/after-load-theme-hook'."
  (run-hooks 'contrib/after-load-theme-hook))

(advice-add #'load-theme :after #'contrib/run-after-load-theme-hook)
(require 'modus-operandi-theme)

(defun vde/modus-operandi ()
  "Enable some Modus Operandi variables and load the theme.
This is used internally by `vde/modus-themes-toggle'."
  (setq modus-operandi-theme-slanted-constructs t
        modus-operandi-theme-bold-constructs t
        modus-operandi-theme-subtle-diffs t
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-section-headings nil
        modus-operandi-theme-scale-headings nil
        modus-operandi-theme-fringes 'subtle ; {nil,'subtle,'intense}
        modus-operandi-theme-mode-line 'moody ; {nil,'3d,'moody}
        modus-operandi-theme-3d-modeline t
        modus-operandi-theme-faint-syntax nil
        modus-operandi-theme-intense-hl-line t
        modus-operandi-theme-intense-paren-match t
        modus-operandi-theme-prompts 'subtle ; {nil,'subtle,'intense}
        modus-operandi-theme-completions 'opinionated ; {nil,'moderate,'opinionated}
        modus-operandi-theme-diffs 'desaturated ; {nil,'desaturated,'fg-only}
        modus-operandi-theme-org-blocks 'greyscale ; {nil,'greyscale,'rainbow}
        modus-operandi-theme-variable-pitch-headings nil
        modus-operandi-theme-rainbow-headings t
        modus-operandi-theme-section-headings nil
        modus-operandi-theme-scale-headings t
        modus-operandi-theme-scale-1 1.05
        modus-operandi-theme-scale-2 1.1
        modus-operandi-theme-scale-3 1.15
        modus-operandi-theme-scale-4 1.2)
  (load-theme 'modus-operandi t))

(defun vde/modus-operandi-custom ()
  "Customize modus-operandi theme"
  (if (member 'modus-operandi custom-enabled-themes)
      (modus-operandi-theme-with-color-variables ; this macro allows us to access the colour palette
        (custom-theme-set-faces
         'modus-operandi
         `(whitespace-tab ((,class (:background "#ffffff" :foreground "#cccccc"))))
         `(whitespace-space ((,class (:background "#ffffff" :foreground "#cccccc"))))
         `(whitespace-hspace ((,class (:background "#ffffff" :foreground "#cccccc"))))
         `(whitespace-newline ((,class (:background "#ffffff" :foreground "#cccccc"))))
         `(whitespace-indentation ((,class (:background "#ffffff" :foreground "#cccccc"))))
         ))))

(add-hook 'contrib/after-load-theme-hook 'vde/modus-operandi-custom)
(vde/modus-operandi)

(defconst font-height 130
  "Default font-height to use.")
(defconst font-family-mono "Ubuntu Mono"
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
(when (member font-family-mono (font-family-list))
  (set-face-attribute 'default nil
                      :family font-family-mono
                      :height font-height)
  (set-face-attribute 'fixed-pitch nil
                      :family font-family-mono))
(when (member font-family-sans (font-family-list))
  (set-face-attribute 'variable-pitch nil
                      :family font-family-sans
                      :weight 'regular))

(set-fontset-font t 'symbol "Apple Color Emoji")
(set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
(set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
(set-fontset-font t 'symbol "Symbola" nil 'append)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)

;; - Reseting garbage collection and file-name-handler values.
(add-hook 'after-init-hook
          `(lambda ()
             (setq gc-cons-threshold 67108864 ; 64mb
                   gc-cons-percentage 0.1
                   file-name-handler-alist file-name-handler-alist-original)
             (garbage-collect)) t)
