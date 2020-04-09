;;; setup-style.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Setup of the style, from font faces to themes
;;; Code:

(when nil
  (progn
    ;;; Interface
    (use-package frame                      ; Frames
      :bind ("C-c w f" . toggle-frame-fullscreen)
      :init
      ;; Kill `suspend-frame'
      (unbind-key "C-x C-z")
      :config (add-to-list 'initial-frame-alist '(fullscreen . maximized)))

    ;; Show buffer position percentage starting from top

    (use-package highlight-numbers
      :hook (prog-mode . highlight-numbers-mode))

    (use-package symbol-overlay
      :defer 4
      :bind
      ("M-s h ." . symbol-overlay-put)
      ("M-s h n" . symbol-overlay-jump-next)
      ("M-s h p" . symbol-overlay-jump-prev)
      :hook (prog-mode . symbol-overlay-mode)
      :config
      (setq symbol-overlay-idle-time 0.2))

    (use-package rainbow-delimiters
      :hook (prog-mode . rainbow-delimiters-mode))

    (use-package rainbow-mode
      :commands rainbow-mode
      :hook (prog-mode . rainbow-mode))

    (use-package visual-fill-column
      :commands visual-fill-column-mode)

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
          (set-dark-theme))))

    ;; Run at every 3600 seconds, after 0s delay
    ;; (run-with-timer 0 3600 'theme-switcher)
    ))

(provide 'setup-style)
