;;; config-appearance.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Appearance configuration
;;; Code:
;; TypeFaceConfiguration
(use-package emacs
  :defer 3
  :bind ("C-c f r" . mu-reset-fonts)
  :commands (mu-reset-fonts)
  :hook (after-init . mu-reset-fonts)
  :config

  (defun mu-reset-fonts ()
    "Reset fonts to my preferences."
    (interactive)
    (when (member "Ubuntu Mono" (font-family-list))
      (set-face-attribute 'default nil
                          :family "Ubuntu Mono"
                          :height font-height))
    (when (member "Ubuntu Sans" (font-family-list))
      (set-face-attribute 'variable-pitch nil
                          :family "Ubuntu Sans"
                          :height font-height
                          :weight 'regular))))
;; -TypeFaceConfiguration

(use-package emacs
  :config
  (setq-default use-file-dialog nil
                use-dialog-box nil
                echo-keystrokes 0.1
                line-number-display-limit-width 10000
                indicate-buffer-boundaries 'left
                indicate-empty-lines +1
                display-time-world-list '(("Europe/London" "London")
                                          ("Europe/Paris" "Paris")
                                          ("America/New_York" "Boston")
                                          ("America/Los_Angeles" "San-Francisco")
                                          ("Asia/Calcutta" "Bangalore")
                                          ("Australia/Brisbane" "Brisbane")))
  (line-number-mode 1)
  (column-number-mode 1)
  (global-hl-line-mode 1)
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z"))
  (global-unset-key (kbd "C-h h")))

;; SafeTheme
(setq custom-safe-themes t)    ; Treat themes as safe
;; -SafeTheme

;; LoadTheme
(defadvice load-theme (before clear-previous-themes activate)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))
;; -LoadTheme

;; UseTheme
(use-package shortbrain-light-theme
  :config
  (load-theme 'shortbrain-light)

  (defun set-light-theme ()
    "Set the light theme with some customization if needed."
    (interactive)
    (use-package shortbrain-light-theme
      :config
      (load-theme 'shortbrain-light t)))

  (defun set-dark-theme ()
    "Set the dark theme with some customization if needed."
    (interactive)
    (use-package shortbrain-theme
      :config
      (load-theme 'shortbrain t)))

  (defun theme-switcher ()
    (interactive)
    (let ((current-hour (string-to-number (format-time-string "%H"))))
      (if (and (> current-hour 6) (< current-hour 20))
          (set-light-theme)
        (set-dark-theme)))))
;; -UseTheme

;; UseMoody
(use-package moody
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

  (use-package minions
    :ensure t
    :config
    (setq-default minions-mode-line-lighter "λ="
                  minions-mode-line-delimiters '("" . "")
                  minions-direct '(flycheck-mode))
    (minions-mode +1))

  (use-package time
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

  (setq-default global-mode-string (remove 'display-time-string global-mode-string)
                mode-line-end-spaces
                (list (propertize " " 'display '(space :align-to (- right 19)))
                      'display-time-string))

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))
;; -UseMoody

(provide 'config-appearance)
;;; config-appearance.el ends here
