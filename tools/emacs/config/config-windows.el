;;; config-windows.el ---  -*- lexical-binding: t; -*-
;; Commentary:
;;; Windows configuration
;; Code:

(setq switch-to-buffer-obey-display-actions t)

(defun vde/save-desktop-no-ask ()
  "Save the desktop without asking questions by modifying the modtime."
  (interactive)
  (require 'desktop)
  (desktop--get-file-modtime)
  (desktop-save (concat desktop-dirname)))
(defun vde/desktop-load ()
  "Load saved desktop"
  (interactive)
  (require 'desktop)
  (desktop-read desktop-dirname))

(bind-key "C-c d s" #'vde/save-desktop-no-ask)
(bind-key "C-c d l" #'vde/desktop-load)

;; Winner
(use-package winner
  :unless noninteractive
  :defer 5
  :config
  (winner-mode 1))
;; -UseWinner

(defun toggle-maximize-buffer ()
  "Maximize buffer"
  (interactive)
  (if (one-window-p)
      (winner-undo)
    (delete-other-windows)))

;; UseAceWindow
(use-package ace-window
  :unless noninteractive
  :commands (ace-window ace-swap-window)
  :bind (("C-x o"   . ace-window)
         ("C-c w w" . ace-window)
         ("C-c w s" . ace-swap-window))
  :config
  (setq-default aw-keys '(?a ?u ?i ?e ?, ?c ?t ?r ?m)
                aw-scope 'frame
                aw-dispatch-always t
                aw-dispatch-alist
                '((?s aw-swap-window "Swap Windows")
                  (?2 aw-split-window-vert "Split Window Vertically")
                  (?3 aw-split-window-horz "Split Window Horizontally")
                  (?? aw-show-dispatch-help))
                aw-minibuffer-flag t
                aw-ignore-current nil
                aw-display-mode-overlay t
                aw-background t))
;; -UseAceWindow

;; UseWindmove
(use-package windmove
  :unless noninteractive
  :commands (windmove-left windmove-right windmove-down windmove-up)
  :bind (("C-M-<up>" . windmove-up)
         ("C-M-<right>" . windmove-right)
         ("C-M-<down>" . windmove-down)
         ("C-M-<left>" . windmove-left)))
;; -UseWindmove

;; UseWindow
(use-package window
  :unless noninteractive
  :commands (shrink-window-horizontally shrink-window enlarge-window-horizontally enlarge-window)
  :bind (("S-C-<left>" . shrink-window-horizontally)
         ("S-C-<right>" . enlarge-window-horizontally)
         ("S-C-<down>" . shrink-window)
         ("S-C-<up>" . enlarge-window)))
;; -UseWindow

;; TODO: Move display-buffer-alist here

(provide 'config-windows)
;;; config-windows ends here
