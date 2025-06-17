;;; passage.el --- -*- lexical-binding: t -*-
;;; Commentary:
;; Code:
(defvar passage-program "passage"
  "The path to the `passage` executable.")

;;;###autoload
(defun passage-get (password-name)
  "Return the password for PASSWORD-NAME from `passage show`."
  (let ((password (shell-command-to-string (concat passage-program " show " (shell-quote-argument password-name)))))
    (string-trim password))) ; Trim whitespace here

(provide 'passage)
;;; passage.el ends here
