;;; vde-simple --- Common functions for my configuration -*- lexical-binding: t -*-

;; Copyright (C) 2025 Vincent Demeester
;; Author: Vincent Demeester <vincent@sbr.pm>

;; This file is NOT part of GNU Emacs.
;;; Commentary:
;;
;; Simple and useful function for a lot of things.
;;
;;; Code:

(defvar vde-simple-override-mode-map (make-sparse-keymap)
  "Key map of `vde-simple-override-mode'.
Enable that mode to have its key bindings to take effect over those of the major mode.")

(define-minor-mode vde-simple-override-mode
  "Enable the `vde-simple-override-mode-map'."
  :init-value nil
  :global t
  :keymap vde-simple-override-mode-map)

(provide 'vde-simple)
;;; vde-simple.el ends here
