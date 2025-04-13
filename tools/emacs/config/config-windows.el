;;; config-windows.el ---  -*- lexical-binding: t; -*-
;; Commentary:
;;; Windows configuration
;; Code:

(setq switch-to-buffer-obey-display-actions t)

(defun vde/window-delete-popup-frame (&rest _)
  "Kill selected selected frame if it has parameter `prot-window-popup-frame'.
Use this function via a hook."
  (when (frame-parameter nil 'vde/window-popup-frame)
    (delete-frame)))

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

;;;###autoload
(defun prot-common-window-small-p ()
  "Return non-nil if window is small.
Check if the `window-width' or `window-height' is less than
`split-width-threshold' and `split-height-threshold',
respectively."
  (or (and (numberp split-width-threshold)
           (< (window-total-width) split-width-threshold))
      (and (numberp split-height-threshold)
           (> (window-total-height) split-height-threshold))))

(defun prot-common-three-or-more-windows-p (&optional frame)
  "Return non-nil if three or more windows occupy FRAME.
If FRAME is non-nil, inspect the current frame."
  (>= (length (window-list frame :no-minibuffer)) 3))

(defun prot-window--get-display-buffer-below-or-pop ()
  "Return list of functions for `prot-window-display-buffer-below-or-pop'."
  (list
   #'display-buffer-reuse-mode-window
   (if (or (prot-common-window-small-p)
           (prot-common-three-or-more-windows-p))
       #'display-buffer-below-selected
     #'display-buffer-pop-up-window)))

(defun prot-window-display-buffer-below-or-pop (&rest args)
  "Display buffer below current window or pop a new window.
The criterion for choosing to display the buffer below the
current one is a non-nil return value for
`prot-common-window-small-p'.

Apply ARGS expected by the underlying `display-buffer' functions.

This as the action function in a `display-buffer-alist' entry."
  (let ((functions (prot-window--get-display-buffer-below-or-pop)))
    (catch 'success
      (dolist (fn functions)
        (when (apply fn args)
          (throw 'success fn))))))

(defvar prot-window-window-sizes
  '( :max-height (lambda () (floor (frame-height) 3))
     :min-height 10
     :max-width (lambda () (floor (frame-width) 4))
     :min-width 20)
  "Property list of maximum and minimum window sizes.
The property keys are `:max-height', `:min-height', `:max-width',
and `:min-width'.  They all accept a value of either a
number (integer or floating point) or a function.")

(defun prot-window--get-window-size (key)
  "Extract the value of KEY from `prot-window-window-sizes'."
  (when-let ((value (plist-get prot-window-window-sizes key)))
    (cond
     ((functionp value)
      (funcall value))
     ((numberp value)
      value)
     (t
      (error "The value of `%s' is neither a number nor a function" key)))))

(defun prot-window-select-fit-size (window &rest _)
  "Select WINDOW and resize it.
The resize pertains to the maximum and minimum values for height
and width, per `prot-window-window-sizes'.

Use this as the `body-function' in a `display-buffer-alist' entry."
  (select-window window)
  (fit-window-to-buffer
   window
   (prot-window--get-window-size :max-height)
   (prot-window--get-window-size :min-height)
   (prot-window--get-window-size :max-width)
   (prot-window--get-window-size :min-width)))

(defun prot-window-shell-or-term-p (buffer &rest _)
  "Check if BUFFER is a shell or terminal.
This is a predicate function for `buffer-match-p', intended for
use in `display-buffer-alist'."
  (when (string-match-p "\\*.*\\(e?shell\\|v?term\\).*" (buffer-name (get-buffer buffer)))
    (with-current-buffer buffer
      ;; REVIEW 2022-07-14: Is this robust?
      (and (not (derived-mode-p 'message-mode 'text-mode))
           (derived-mode-p 'eshell-mode 'shell-mode 'comint-mode 'fundamental-mode)))))

;; (setq display-buffer-alist
;;       `(;; Default to no window
;; 	("\\`\\*Async Shell Command\\*\\'"
;; 	 (display-buffer-no-window))
;; 	("\\`\\*Warnings\\*\\'"
;;          (display-buffer-no-window)
;; 	 (allow-no-window . t))
;; 	("\\`\\*\\(Compile-Log\\|Org Links\\)\\*\\'"
;;          (display-buffer-no-window))
;; 	;; bottom side window
;;         ("\\*Org \\(Select\\|Note\\)\\*" ; the `org-capture' key selection and `org-add-log-note'
;;          (display-buffer-in-side-window)
;;          (dedicated . t)
;;          (side . bottom)
;;          (slot . 0)
;;          (window-parameters . ((mode-line-format . none))))
;; 	;; bottom buffer (NOT side window)
;;         ((or . ((derived-mode . flymake-diagnostics-buffer-mode)
;;                 (derived-mode . flymake-project-diagnostics-mode)
;;                 (derived-mode . messages-buffer-mode)
;;                 (derived-mode . backtrace-mode)))
;;          (display-buffer-reuse-mode-window display-buffer-at-bottom)
;;          (window-height . 0.3)
;;          (dedicated . t)
;;          (preserve-size . (t . t)))
;; 	;; ((or . ((derived-mode . Man-mode)
;; 	;; 	(derived-mode . woman-mode)
;; 	;; 	"\\*\\(Man\\|woman\\).*"))
;; 	;;  (display-buffer-reuse-mode-window display-buffer-below-selected)
;;         ;;  (window-height . 0.3) ; note this is literal lines, not relative
;;         ;;  (dedicated . t)
;;         ;;  (preserve-size . (t . t)))
;; 	;; below current window
;; 	("\\(\\*Capture\\*\\|CAPTURE-.*\\)"
;;          (display-buffer-reuse-mode-window display-buffer-below-selected))
;; 	((derived-mode . reb-mode) ; M-x re-builder
;;          (display-buffer-reuse-mode-window display-buffer-below-selected)
;;          (window-height . 4) ; note this is literal lines, not relative
;;          (dedicated . t)
;;          (preserve-size . (t . t)))
;; 	((or . ((derived-mode . occur-mode)
;;                 (derived-mode . grep-mode)
;;                 (derived-mode . Buffer-menu-mode)
;;                 (derived-mode . log-view-mode)
;; 		(derived-mode . helpful-mode)
;;                 (derived-mode . help-mode) ; See the hooks for `visual-line-mode'
;;                 "\\*\\(|Buffer List\\|Occur\\|vc-change-log\\).*"
;;                 prot-window-shell-or-term-p
;;                 ,world-clock-buffer-name))
;;          (prot-window-display-buffer-below-or-pop)
;;          (dedicated . t)
;;          (body-function . prot-window-select-fit-size))
;; 	))

(use-package auto-side-windows
  :custom
  ;; Top side window configurations
  (auto-side-windows-top-buffer-names
   '("^\\*Backtrace\\*$" "^\\*Compile-Log\\*$" "^COMMIT_EDITMSG$"
     "^\\*Org Src.*\\*" "^\\*Agenda Commands\\*$" "^\\*Org Agenda\\*$"
     "^\\*Quick Help\\*$" "^\\*Multiple Choice Help\\*$" "^\\*TeX Help\\*$"
     "^\\*TeX errors\\*$" "^\\*Warnings\\*$" "^\\*diff-hl\\*$"
     "^\\*Process List\\*$"))
  (auto-side-windows-top-buffer-modes
   '(flymake-diagnostics-buffer-mode locate-mode occur-mode grep-mode
                                     xref--xref-buffer-mode))

  ;; Bottom side window configurations
  (auto-side-windows-bottom-buffer-names
   '("^\\*.*eshell.*\\*$" "^\\*.*shell.*\\*$" "^\\*.*term.*\\*$"
     "^\\*.*vterm.*\\*$" "^\\*.*eat.*\\*$"))
  (auto-side-windows-bottom-buffer-modes
   '(eshell-mode shell-mode term-mode vterm-mode comint-mode debugger-mode))

  ;; Right side window configurations
  (auto-side-windows-right-buffer-names
   '("^\\*eldoc.*\\*$" "^\\*info\\*$" "^\\*Metahelp\\*$"))
  (auto-side-windows-right-buffer-modes
   '(Info-mode TeX-output-mode pdf-view-mode eldoc-mode help-mode
               helpful-mode shortdoc-mode))

  ;; Example: Custom parameters for top windows (e.g., fit height to buffer)
  ;; (auto-side-windows-top-alist '((window-height . fit-window-to-buffer)))
  ;; (auto-side-windows-top-window-parameters '((mode-line-format . ...))) ;; Adjust mode-line

  ;; Maximum number of side windows on the left, top, right and bottom
  (window-sides-slots '(1 1 1 1)) ; Example: Allow one window per side

  ;; Force left and right side windows to occupy full frame height
  (window-sides-vertical t)
  (window-combination-resize t)

  ;; Make changes to tab-/header- and mode-line-format persistent when toggleling windows visibility
  (window-persistent-parameters
   (append window-persistent-parameters
           '((tab-line-format . t)
             (header-line-format . t)
             (mode-line-format . t))))
  :bind ;; Example keybindings (adjust prefix as needed)
  (:map global-map ; Or your preferred keymap prefix
        ("C-c w t" . auto-side-windows-display-buffer-top)
        ("C-c w b" . auto-side-windows-display-buffer-bottom)
        ("C-c w l" . auto-side-windows-display-buffer-left)
        ("C-c w r" . auto-side-windows-display-buffer-right)
        ("C-c w T" . auto-side-windows-toggle-side-window)) ; Toggle current buffer in/out of side window
  :hook
  (after-init . auto-side-windows-mode)) ; Activate the mode

(use-package popper
  :after auto-side-windows ; Ensure auto-side-windows variables are defined
  :hook (auto-side-windows-mode . popper-mode) ; Activate popper alongside
  :custom
  ;; Tell Popper to consider buffers matching auto-side-windows rules as popups
  (popper-reference-buffers
   (append auto-side-windows-top-buffer-names auto-side-windows-top-buffer-modes
           auto-side-windows-left-buffer-names auto-side-windows-left-buffer-modes
           auto-side-windows-right-buffer-names auto-side-windows-right-buffer-modes
           auto-side-windows-bottom-buffer-names auto-side-windows-bottom-buffer-modes))
  ;; Optional: Don't let Popper decide where to display, auto-side-windows handles that
  (popper-display-control nil) ; Or 'user if you prefer popper commands for display
  :config
  (require 'popper-echo)
  (popper-mode +1) ; Enable popper-mode
  (popper-echo-mode +1) ; Optional: echo area notifications
  :bind
  (("C-`" . popper-toggle)
   ("C-M-`" . popper-toggle-type)
   ("M-`" . popper-cycle)))
;; TODO: Move display-buffer-alist here

(provide 'config-windows)
;;; config-windows ends here
