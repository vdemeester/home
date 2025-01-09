;;; config-mu4e.el -- mu emacs client configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package mu4e
  :commands (mu4e)
  :config
  (setopt mu4e-completing-read-function completing-read-function))

(use-package consult-mu
  :commands (consult-mu))

(provide 'config-mu4e)
;;; config-mu4e.el ends here
