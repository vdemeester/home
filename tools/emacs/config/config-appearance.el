;;; config-appearance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Appearance configuration
;;; Code:

(use-package emacs
  :defer 3
  :bind ("C-c f r" . mu-reset-fonts)
  :commands (mu-reset-fonts)
  :hook (after-init . mu-reset-fonts)
  :config
  ;; For displaying emojies 😛🦁
  (set-fontset-font t 'symbol "Apple Color Emoji")
  (set-fontset-font t 'symbol "Noto Color Emoji" nil 'append)
  (set-fontset-font t 'symbol "Segoe UI Emoji" nil 'append)
  (set-fontset-font t 'symbol "Symbola" nil 'append)
  (defun mu-reset-fonts ()
    "Reset fonts to my preferences."
    (interactive)
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
    (set-fontset-font t 'symbol "Symbola" nil 'append)))

(use-package emacs
  :config
  (setq-default use-file-dialog nil
                use-dialog-box nil
                echo-keystrokes 0.1
                line-number-display-limit-width 10000
                indicate-buffer-boundaries 'left
                indicate-empty-lines +1)
  (line-number-mode 1)
  (column-number-mode 1)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h"))
  ;; let's enable it for all programming major modes
  (add-hook 'prog-mode-hook #'hl-line-mode)
  ;; and for all modes derived from text-mode
  (add-hook 'text-mode-hook #'hl-line-mode))

(use-package frame
  :unless noninteractive
  :commands vde/cursor-type-mode
  :config
  (setq-default cursor-type 'box)
  (setq-default cursor-in-non-selected-windows '(bar . 2))
  (setq-default blink-cursor-blinks 50)
  (setq-default blink-cursor-interval nil) ; 0.75 would be my choice
  (setq-default blink-cursor-delay 0.2)

  (blink-cursor-mode -1)

  (define-minor-mode vde/cursor-type-mode
    "Toggle between static block and pulsing bar cursor."
    :init-value nil
    :global t
    (if vde/cursor-type-mode
        (progn
          (setq-local blink-cursor-interval 0.75
                      cursor-type '(bar . 2)
                      cursor-in-non-selected-windows 'hollow)
          (blink-cursor-mode 1))
      (dolist (local '(blink-cursor-interval
                       cursor-type
                       cursor-in-non-selected-windows))
        (kill-local-variable `,local))
      (blink-cursor-mode -1))))
(use-package emacs
  :config
  (setq-default custom-safe-themes t)
  (setq-default custom--inhibit-theme-enable nil)

  (defun vde/before-load-theme (&rest args)
    "Clear existing theme settings instead of layering them.
Ignores `ARGS'."
    (mapc #'disable-theme custom-enabled-themes))

  (advice-add 'load-theme :before #'vde/before-load-theme))

(use-package emacs
  :config
  (setq window-divider-default-right-width 1)
  (setq window-divider-default-bottom-width 1)
  (setq window-divider-default-places 'right-only)
  :hook (after-init . window-divider-mode))
(use-package tab-bar
  :unless noninteractive
  :config
  (setq-default tab-bar-close-button-show nil)
  (setq-default tab-bar-close-last-tab-choice 'tab-bar-mode-disable)
  (setq-default tab-bar-close-tab-select 'recent)
  (setq-default tab-bar-new-tab-choice t)
  (setq-default tab-bar-new-tab-to 'right)
  (setq-default tab-bar-position nil)
  (setq-default tab-bar-show t)
  (setq-default tab-bar-tab-hints nil)
  (setq-default tab-bar-tab-name-function 'vde/tab-bar-tab-name)

  (defun vde/tab-bar-tab-name ()
    "Generate tab name from the buffer of the selected window *or* projectile."
    (cond
     ((boundp 'projectile-project-name) (if (string-equal (projectile-project-name) "-")
                                            (tab-bar-tab-name-current-with-count)
                                          (projectile-project-name)))
     ((project-current) (let ((project-path (vde-project--project-current)))
                          (cond ((string-prefix-p "~/src" project-path)
                                 (directory-file-name (file-relative-name project-path "~/src")))
                                ((string-prefix-p "~/desktop" project-path)
                                 (directory-file-name (file-relative-name project-path "~/desktop")))
                                ((string-prefix-p "/etc" project-path)
                                 (directory-file-name (file-relative-name project-path "/etc")))
                                (t
                                 (file-relative-name project-path)))))
     (t (tab-bar-tab-name-current-with-count))))

  (defun vde/complete-tab-bar-tab-dwim ()
    "Do-What-I-Mean function for getting to a `tab-bar-mode' tab.
If no other tab exists, create one and switch to it.  If there is
one other tab (so two in total) switch to it without further
questions.  Else use completion to select the tab to switch to."
    (interactive)
    (let ((tabs (mapcar (lambda (tab)
                          (alist-get 'name tab))
                        (tab-bar--tabs-recent))))
      (cond ((eq tabs nil)
             (tab-new))
            ((eq (length tabs) 1)
             (tab-next))
            (t
             (tab-bar-switch-to-tab
              (completing-read "Select tab: " tabs nil t))))))

  :bind (("C-x t t" . vde/complete-tab-bar-tab-dwim)
         ("C-x t s" . tab-switcher)))

(use-package moody
  :unless noninteractive
  :config
  (setq-default x-underline-at-descent-line t
                ;; Show buffer position percentage starting from top
                mode-line-percent-position '(-3 "%o"))

  (setq-default mode-line-format
                '("%e"
                  mode-line-front-space
                  mode-line-client
                  mode-line-modified
                  mode-line-remote
                  mode-line-frame-identification
                  mode-line-buffer-identification " " mode-line-position
                  (vc-mode vc-mode)
                  (multiple-cursors-mode mc/mode-line)
                  " " mode-line-modes
                  mode-line-end-spaces))

  (setq-default global-mode-string (remove 'display-time-string global-mode-string)
                mode-line-end-spaces
                (list (propertize " " 'display '(space :align-to (- right 19)))
                      'display-time-string))
  (advice-add #'vc-git-mode-line-string :filter-return #'my-replace-git-status)
  (defun my-replace-git-status (tstr)
    (let* ((tstr (replace-regexp-in-string "Git" "" tstr))
           (first-char (substring tstr 0 1))
           (rest-chars (substring tstr 1)))
      (cond
       ((string= ":" first-char) ;;; Modified
        (replace-regexp-in-string "^:" "~ " tstr))
       ((string= "-" first-char) ;; No change
        (replace-regexp-in-string "^-" "- " tstr))
       (t tstr))))
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :unless noninteractive
  :config
  (setq-default minions-mode-line-lighter "λ="
                minions-mode-line-delimiters '("" . "")
                minions-direct '(flycheck-mode))
  (minions-mode +1))

(use-package time
  :unless noninteractive
  :config
  (setq-default display-time-24hr-format t
                display-time-day-and-date t
                display-time-world-list '(("Europe/Paris" "Paris")
                                          ("Europe/London" "London")
                                          ("America/New_York" "Boston")
                                          ("America/Los_Angeles" "San Francisco")
                                          ("Asia/Calcutta" "Bangalore")
                                          ("Australia/Brisbane" "Brisbane"))
                display-time-string-forms
                '((format "%s %s %s, %s:%s"
                          dayname
                          monthname day
                          24-hours minutes)))
  (display-time))

(use-package lin
  :unless noninteractive
  :config
  (setq lin-face 'lin-blue)
  (setq lin-mode-hooks
	    '(bongo-mode-hook
          dired-mode-hook
          elfeed-search-mode-hook
          git-rebase-mode-hook
          ibuffer-mode-hook
          ilist-mode-hook
          ledger-report-mode-hook
          log-view-mode-hook
          magit-log-mode-hook
          mu4e-headers-mode
          notmuch-search-mode-hook
          notmuch-tree-mode-hook
          occur-mode-hook
          org-agenda-mode-hook
          tabulated-list-mode-hook))
  (lin-global-mode))

(use-package emacs
  :unless noninteractive
  :config
  (set-frame-parameter nil 'alpha-background 96)
  (add-to-list 'default-frame-alist '(alpha-background . 96)))

(use-package tooltip
  :unless noninteractive
  :config
  (setq tooltip-delay 0.5)
  (setq tooltip-short-delay 0.5)
  (setq x-gtk-use-system-tooltips nil)
  (setq tooltip-frame-parameters
        '((name . "tooltip")
          (internal-border-width . 6)
          (border-width . 0)
          (no-special-glyphs . t)))
  :hook (after-init-hook . tooltip-mode))

(use-package alert
  :config
  (setq alert-default-style 'libnotify))

(provide 'config-appearance)
;;; config-appearance.el ends here
