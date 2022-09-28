;;; config-windows.el ---  -*- lexical-binding: t; -*-
;; Commentary:
;;; Windows configuration
;; Code:

;; By default, emacs distinguishes between automatic and manual window switching.
;; It can make it weird, so… let's make sure it has the same behavior for both
;; See: https://www.masteringemacs.org/article/demystifying-emacs-window-manager
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)

(use-package winner
  :unless noninteractive
  :defer 5
  :config
  (winner-mode 1))

(use-package windmove
  :unless noninteractive
  :commands (windmove-left windmove-right windmove-down windmove-up)
  :bind (("C-M-<up>" . windmove-up)
         ("C-M-<right>" . windmove-right)
         ("C-M-<down>" . windmove-down)
         ("C-M-<left>" . windmove-left)))

(use-package emacs
  :unless noninteractive
  :bind (("M-o" . other-window))
  :config
  ;; left, top, right, bottom
  (setq window-sides-slots '(1 1 1 2))
  ;; Configure `display-buffer' behaviour for some special buffers
  ;; To get a list of action, `C-u C-h a ^display-buffer-[^-]'.
  (setq-default display-buffer-alist
		'(
		  ;; helpful buffers are displayed on top
                  ("\\*\\(helpful\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . top)
                   (slot . 0)
		   (display-buffer-reuse-window display-buffer-pop-up-window)
		   (inhibit-same-window . t))
		  ;; compilation buffer rules
		  ;; compilation can be "per project", like `*home-compilation*'.
		  ("\\*.*Compilation.*\\*"
		   display-buffer-reuse-window)
		  ;; `*info*' should be display on a side window, with a 80 width
		  ;; and no-delete-other-windows (`C-x 1' will not hide this one)
		  ("\\*info\\*"
		   (display-buffer-in-side-window)
		   (side . right)
		   (slot . 0)
		   (window-width . 80)
		   (window-parameters
		    (no-delete-other-windows . t)))
		  ;; vterm rules : reuse a window with vtrm or vterm-copy-mode
		  ("\\*vterm.*\\*"display-buffer-reuse-mode-window
		   ;; change to `t' to not reuse same window
		   (inhibit-same-window . nil)
		   (mode vterm-mode vterm-copy-mode))
		  ;; eshell/shells ruls
		  ("\\*.*e?shell.*"
		   display-buffer-in-direction
		   (direction . bottom)
		   (window . root)
		   (window-height . 0.3))
		  ;; Collecting "relatively random" buffers in the same window
		  (,(rx (| "*xref*"
			   "*grep*"
			   "*Occur*"))
		   display-buffer-reuse-window
		   (inhibit-same-window . nil))
		  ;; FIXME: this is an example, tailor this for go dev (and rust, …)
		  ("^test[-_]"
		   display-buffer-in-direction
		   (direction . right))
		  ))
  )

(provide 'config-windows)
;;; config-windows ends here
