;;; config-buffers.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Buffer related configurations
;;; Code:

(use-package popper
  :commands (popper-mode)
  :bind ((("C-`" . popper-toggle-latest)
          ("M-`" . popper-cycle)
          ("C-M-`" . popper-toggle-type)))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$" "\\*Pp Eval Output\\*$"
          "\\*Compile-Log\\*"
          "\\*Completions\\*"
          "\\*Warnings\\*"
          "\\*Async Shell Command\\*"
          "\\*Apropos\\*"
          "\\*Backtrace\\*"
          "\\*Calendar\\*"
          "\\*Embark Actions\\*"
          "\\*Finder\\*"
          "\\*Kill Ring\\*"

          bookmark-bmenu-mode
          comint-mode
          compilation-mode
          help-mode helpful-mode
          tabulated-list-mode
          Buffer-menu-mode

          gnus-article-mode devdocs-mode
          grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode
          ivy-occur-mode ivy-occur-grep-mode
          process-menu-mode list-environment-mode cargo-process-mode
          youdao-dictionary-mode osx-dictionary-mode fanyi-mode

          "^\\*eshell.*\\*.*$" eshell-mode
          "^\\*shell.*\\*.*$"  shell-mode
          "^\\*terminal.*\\*.*$" term-mode
          "^\\*vterm.*\\*.*$"  vterm-mode

          "\\*DAP Templates\\*$" dap-server-log-mode
          "\\*ELP Profiling Restuls\\*" profiler-report-mode
          "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$"
          "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
          "\\*[Wo]*Man.*\\*$"
          "\\*ert\\*$" overseer-buffer-mode
          "\\*gud-debug\\*$"
          "\\*lsp-help\\*$" "\\*lsp session\\*$"
          "\\*quickrun\\*$"
          "\\*tldr\\*$"
          "\\*vc-.*\\*$"
          "^\\*elfeed-entry\\*$"
          "^\\*macro expansion\\**"

          "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
          "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
          "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
          "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
          "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
          rustic-cargo-outdated-mode rustic-cargo-test-moed))
  (setq popper-group-function #'popper-group-by-project)
  :config
  (popper-echo-mode 1))

(use-package emacs
  :unless noninteractive
  :config
  ;; Configure `display-buffer' behaviour for some special buffers
  (setq-default display-buffer-alist
                '(;; bottom side window
                  ;; ("\\*e?shell.*"
                  ;;  (display-buffer-in-side-window)
                  ;;  (window-height . 0.25)
                  ;;  (side . bottom)
                  ;;  (slot . -1))
                  ;; ("\\*v?term.*"
                  ;;  (display-buffer-in-side-window)
                  ;;  (window-height . 0.25)
                  ;;  (side . bottom)
                  ;;  (slot . -1))
                  (".*\\*\\(Completions\\).*"
                   (display-buffer-in-side-window)
                   (window-height . 0.16)
                   (side . bottom)
                   (slot . 0)
                   (window-parameters . ((no-other-window . t))))
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
                  ("\\*.*\\(compilation\\|go test\\).*"
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
                   (window-width . 0.40)
                   (side . right)
                   (slot . 0)
                   (window-parameters . ((no-other-window . t)
                                         (mode-line-format . (" "
                                                              mode-line-buffer-identification)))))
                  ;; ("\\*Custom.*"
                  ;;  (display-buffer-in-side-window)
                  ;;  (window-width . 0.20)
                  ;;  (side . right)
                  ;;  (slot . 2))
                  ("\\*Embark Occur.*"
                   (display-buffer-at-bottom))))
  (setq window-sides-vertical nil)
  (setq window-combination-resize t) ; Size new windows proportionally
  :bind (("C-x +" . balance-windows-area)
         ("<C-f7>" . window-toggle-side-windows)))

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
                                                shell-command-history
                                                projectile-project-command-history))
  (savehist-mode 1))

(use-package uniquify
  :unless noninteractive
  :config
  (setq-default uniquify-buffer-name-style 'post-forward
                uniquify-separator ":"
                uniquify-ignore-buffers-re "^\\*"
                uniquify-after-kill-buffer-p t))

(use-package ibuffer
  :unless noninteractive
  :commands (ibuffer)
  :bind (("C-x C-b" . ibuffer)
         ([remap list-buffers] . ibuffer))
  :config
  (setq-default ibuffer-expert t
                ibuffer-filter-group-name-face 'font-lock-doc-face
                ibuffer-default-sorting-mode 'filename/process
                ibuffer-use-header-line t
                ibuffer-show-empty-filter-groups nil)
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

(use-package ibuffer-vc
  :unless noninteractive
  :commands (ibuffer-vc-set-filter-groups-by-vc-root)
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'filename/process)
                       (ibuffer-do-sort-by-filename/process)))))

(provide 'config-buffers)
;;; config-buffers.el ends here
