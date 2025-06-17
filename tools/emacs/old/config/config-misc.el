;;; config-misc.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Miscellaneous modes configuration
;;; Code:

;; (use-package password-store
;;   :custom
;;   (password-store-executable "passage"))

(defvar passage-program "passage"
  "The path to the `passage` executable.")

(defun passage-get (password-name)
  "Return the password for PASSWORD-NAME from `passage show`."
  (let ((password (shell-command-to-string (concat passage-program " show " (shell-quote-argument password-name)))))
    (string-trim password))) ; Trim whitespace here

(use-package helpful
  :unless noninteractive
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h M" . helpful-macro)
         ("C-c h S" . helpful-at-point)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h C" . helpful-command)))

(provide 'config-misc)
;;; config-misc.el ends here
