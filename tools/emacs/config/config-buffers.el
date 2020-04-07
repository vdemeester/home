;;; -*- lexical-binding: t; -*-

;; UseDisplayBuffers
(use-package emacs
  :unless noninteractive
  :config
  ;; Configure `display-buffer' behaviour for some special buffers
  (setq-default display-buffer-alist
                '(;; bottom side window
                  ("\\*e?shell.*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . bottom)
                   (slot . -1))
                  ("\\*v?term.*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . bottom)
                   (slot . -1))
                  ("\\*\\(Backtrace\\|Warnings\\|Compile-Log\\|[Hh]elp\\|Messages\\)\\*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . bottom)
                   (slot . 0))
                  ("\\*\\(helpful\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . bottom)
                   (slot . 0))
                  ("\\*\\(compilation\\|go test\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . bottom)
                   (slot . 0))
                  ("\\*\\(ielm\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.25)
                   (side . bottom)
                   (slot . 1))
                  ;; right side window
                  ("\\*wclock*"
                   (display-buffer-in-side-window)
                   (window-width . 0.20)
                   (side . right)
                   (slot . -1))
                  ("\\*undo-tree*"
                   (display-buffer-in-side-window)
                   (window-width . 0.20)
                   (side . right)
                   (slot . -1))
                  ("\\*\\(Flycheck\\|Package-Lint\\).*"
                   (display-buffer-in-side-window)
                   (window-width . 0.20)
                   (side . right)
                   (slot . 0)
                   (window-parameters . ((no-other-window . t)
                                         (mode-line-format . (" "
                                                              mode-line-buffer-identification)))))
                  ("\\*Faces\\*"
                   (display-buffer-in-side-window)
                   (window-width . 0.20)
                   (side . right)
                   (slot . 1)
                   (window-parameters . ((no-other-window . t)
                                         (mode-line-format . (" "
                                                              mode-line-buffer-identification)))))
                  ("\\*Custom.*"
                   (display-buffer-in-side-window)
                   (window-width . 0.20)
                   (side . right)
                   (slot . 2))))
  (setq window-sides-vertical nil)
  (setq window-combination-resize t) ; Size new windows proportionally
  :bind (("C-x +" . balance-windows-area)
         ("<f7>" . window-toggle-side-windows)))
;; -UseDisplayBuffer

;; UseSaveHist
(use-package savehist
  :unless noninteractive
  :config
  (setq-default history-length 10000
                savehist-save-minibuffer-history t
                savehist-autosave-interval 180
                savehist-additional-variables '(extended-command-history
                                                search-ring
                                                regexp-search-ring
                                                comint-input-ring
                                                compile-history
                                                last-kbd-macro
                                                shell-command-history))
  (savehist-mode 1))
;; -UseSaveHist

(when nil
  (progn
    ;; Show the minibuffer depth (when larger than 1)




    (use-package uniquify
      :config
      (setq-default uniquify-buffer-name-style 'post-forward
                    uniquify-separator ":"
                    uniquify-ignore-buffers-re "^\\*"
                    uniquify-after-kill-buffer-p t))

    (use-package ibuf-ext                   ; Extensions for Ibuffer
      :config
      ;; Do not show empty groups
      (setq-default ibuffer-show-empty-filter-groups nil))

    (use-package ibuffer                    ; Buffer management
      :bind (("C-x C-b" . ibuffer)
             ([remap list-buffers] . ibuffer))
      :config
      (setq-default ibuffer-expert t
                    ibuffer-filter-group-name-face 'font-lock-doc-face
                    ibuffer-default-sorting-mode 'filename/process
                    ibuffer-use-header-line t)
      ;; Use human readable Size column instead of original one
      (define-ibuffer-column size-h
        (:name "Size" :inline t)
        (cond
         ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
         ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
         (t (format "%8d" (buffer-size)))))

      (setq ibuffer-formats
            '((mark modified read-only " "
                    (name 18 18 :left :elide)
                    " "
                    (size-h 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " "
                    filename-and-process)
              (mark modified read-only " "
                    (name 18 18 :left :elide)
                    " "
                    (size 9 -1 :right)
                    " "
                    (mode 16 16 :left :elide)
                    " "
                    (vc-status 16 16 :left)
                    " "
                    filename-and-process))))

    (use-package ibuffer-vc                 ; Group buffers by VC project and status
      :defer 2
      :init (add-hook 'ibuffer-hook
                      (lambda ()
                        (ibuffer-vc-set-filter-groups-by-vc-root)
                        (unless (eq ibuffer-sorting-mode 'filename/process)
                          (ibuffer-do-sort-by-filename/process)))))

    ))
(provide 'setup-buffers)
