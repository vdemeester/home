;;; 02-help.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; help and info mode setup
;;; Code:
(use-package helpful
  :unless noninteractive
  :bind (("C-h f" . helpful-callable)
         ("C-h F" . helpful-function)
         ("C-h M" . helpful-macro)
         ("C-c h S" . helpful-at-point)
         ("C-h k" . helpful-key)
         ("C-h v" . helpful-variable)
         ("C-h C" . helpful-command)))

;;; 02-help.el ends here
