;;; 00-base.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Emacs *absolute* base configuration
;;; Code:

(put 'overwrite-mode 'disabled t) ;; I don't really want to use overwrite-mod, ever

(setq echo-keystrokes 0.1) ;; display command keystrokes quickly

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-h h"))

;; Custom file management
(defconst vde/custom-file (locate-user-emacs-file "custom.el")
  "File used to store settings from Customization UI.")

(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

(use-package cus-edit
  :config
  (setq
   custom-file vde/custom-file
   custom-buffer-done-kill nil          ; Kill when existing
   custom-buffer-verbose-help nil       ; Remove redundant help text
   custom-unlispify-tag-names nil       ; Show me the real variable name
   custom-unlispify-menu-entries nil)
  (unless (file-exists-p custom-file)
    (write-region "" nil custom-file))

  (load vde/custom-file 'no-error 'no-message))

(provide '00-base)
;;; 00-base.el ends here
