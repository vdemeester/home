;;; config-windows.el ---  -*- lexical-binding: t; -*-
;; Commentary:
;;; Windows configuration
;; Code:

;; Winner
(use-package winner
  :unless noninteractive
  :defer 5
  :config
  (winner-mode 1))
;; -UseWinner

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
  :bind (("C-s-<up>" . windmove-up)
         ("C-s-<right>" . windmove-right)
         ("C-s-<down>" . windmove-down)
         ("C-s-<left>" . windmove-left)))
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

(use-package zoom-window
  :commands (zoom-window-zoom)
  :bind (("C-x C-z" . zoom-window-zoom))
  :config
  (setq zoom-window-mode-line-color "#8ac7ff"))

(provide 'config-windows)
;;; config-windows ends here
