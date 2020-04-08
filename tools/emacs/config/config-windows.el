;;; -*- lexical-binding: t; -*-

;; Winner
(use-package winner
  :unless noninteractive
  :defer 5
  :config
  (winner-mode 1))
;; -UseWinner

;; UseAceWindow
(use-package ace-window
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
  :commands (windmove-left windmove-right windmove-down windmove-up)
  :bind (("M-<left>" . windmove-left)
         ("M-<down>" . windmove-down)
         ("M-<up>" . windmove-up)
         ("M-<right>" . windmove-right)))
;; -UseWindmove

(defun vde/window-split-toggle ()
  "Toggle between horizontal and vertical split with two windows."
  (interactive)
  (if (> (length (window-list)) 2)
      (error "Can't toggle with more than 2 windows!")
    (let ((func (if (window-full-height-p)
                    #'split-window-vertically
                  #'split-window-horizontally)))
      (delete-other-windows)
      (funcall func)
      (save-selected-window
        (other-window 1)
        (switch-to-buffer (other-buffer))))))

;;(bind-key "C-c w t" #'vde/window-split-toggle)

(use-package eyebrowse
  :commands (eyebrowse-switch-to-window-config)
  :bind (("C-c w s" . eyebrowse-switch-to-window-config)
         ("C-c w k" . eyebrowse-close-window-config)
         ("C-c w w" . eyebrowse-last-window-config)
         ("C-c w n" . eyebrowse-next-window-config)
         ("C-c w p" . eyebrowse-prev-window-config))
  :config
  (setq-default eyebrowse-mode-line-separator " "
                eyebrowse-mode-line-style 'always
                eyebrowse-new-workspace t
                eyebrowse-wrap-around t)
  (eyebrowse-mode 1))

(provide 'setup-windows)
