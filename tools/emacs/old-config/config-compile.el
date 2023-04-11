;;; config-compile.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Generic compilation configuration
;;; Code:

;; UseCompile
(use-package compile
  :unless noninteractive
  :commands (compile)
  :preface
  (autoload 'ansi-color-apply-on-region "ansi-color")

  (defvar compilation-filter-start)

  (defun vde/colorize-compilation-buffer ()
    (unless (or (derived-mode-p 'grep-mode)
                (derived-mode-p 'ag-mode)
                (derived-mode-p 'rg-mode))
      (let ((inhibit-read-only t))
        (ansi-color-apply-on-region compilation-filter-start (point)))))
  (defun vde/goto-address-mode ()
    (unless (or (derived-mode-p 'grep-mode)
                (derived-mode-p 'ag-mode)
                (derived-mode-p 'rg-mode))
      (goto-address-mode t)))
  :config
  (setq-default compilation-scroll-output t
                ;; I'm not scared of saving everything.
                compilation-ask-about-save nil
                ;; Automatically scroll and jump to the first error
                ;; compilation-scroll-output 'next-error
                ;; compilation-scroll-output 'first-error
                ;; compilation-auto-jump-to-first-error t
                ;; Skip over warnings and info messages in compilation
                compilation-skip-threshold 2
                ;; Don't freeze when process reads from stdin
                compilation-disable-input t
                ;; Show three lines of context around the current message
                compilation-context-lines 3
                )
  (add-hook 'comint-output-filter-functions
            'comint-watch-for-password-prompt)
  (setq-default comint-password-prompt-regexp
                (concat
                 "\\("
                 "^Enter passphrase.*:"
                 "\\|"
                 "^Repeat passphrase.*:"
                 "\\|"
                 "[Pp]assword for '[a-z0-9_-.]+':"
                 "\\|"
                 "\\[sudo\\] [Pp]assword for [a-z0-9_-.]+:"
                 "\\|"
                 "[a-zA-Z0-9]'s password:"
                 "\\|"
                 "^[Pp]assword:"
                 "\\|"
                 "^[Pp]assword (again):"
                 "\\|"
                 ".*\\([Ww]ork\\|[Pp]ersonal\\).* password:"
                 "\\|"
                 "Password for '([^()]+)' GNOME keyring"
                 "\\|"
                 "Password for 'http.*github.*':"
                 "\\)"))
  (add-hook 'compilation-filter-hook #'vde/colorize-compilation-buffer)
  (add-hook 'compilation-mode-hook #'vde/goto-address-mode))

(provide 'config-compile)
;;; config-compile.el ends here
