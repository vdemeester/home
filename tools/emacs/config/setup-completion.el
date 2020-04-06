;;; -*- lexical-binding: t; -*-
(use-package ivy
  :delight
  :bind (("C-x b" . vde/switch-buffer)
         ("C-x B" . ivy-switch-buffer)
         ("M-u" . ivy-resume)    ;Override the default binding for `upcase-word'
         ("C-c C-w p" . ivy-push-view) ;Push window configuration to `ivy-views'
         ("C-c C-w P" . ivy-pop-view)  ;Remove window configuration from `ivy-views'
         ("C-c C-w s" . ivy-switch-view) ; Switch window configuration to `ivy-views'
         :map ivy-occur-mode-map
         ("f" . forward-char)
         ("b" . backward-char)
         ("n" . ivy-occur-next-line)
         ("p" . ivy-occur-previous-line)
         ("<C-return>" . ivy-occur-press))
  :hook
  (ivy-occur-mode . hl-line-mode)
  :config
  (setq ivy-count-format "%d/%d "
        ivy-height-alist '((t lambda (_caller) (/ (window-height) 4)))
        ivy-use-virtual-buffers t
        ivy-virtual-abbreviate 'full ;Show the full virtual file paths
        ivy-wrap nil
        ivy-re-builders-alist '((counsel-M-x . ivy--regex-fuzzy)
                                (t . ivy--regex-plus))
        ivy-display-style 'fancy
        ivy-use-selectable-prompt t
        ivy-fixed-height-minibuffer nil
        ivy-extra-directories '("../" "./")
        )
  (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur)
  (ivy-set-occur 'swiper 'swiper-occur)
  (ivy-set-occur 'swiper-isearch 'swiper-occur)
  (ivy-mode 1)
  (progn
    (defun vde/switch-buffer (arg)
      "Custom switch to buffer.
With universal argument ARG or when not in project, rely on
`ivy-switch-buffer'.
Otherwise, use `counsel-projectile-switch-project'."
      (interactive "P")
      (if (or arg
              (not (projectile-project-p)))
          (ivy-switch-buffer)
        (counsel-projectile-switch-to-buffer)))
    ;; Disable ido
    (with-eval-after-load 'ido
      (ido-mode -1)
      ;; Enable ivy
      (ivy-mode 1))
    ))

(use-package counsel
  :after ivy
  :bind (("M-i" . counsel-semantic-or-imenu)
         ("C-x C-r" . counsel-recentf)
         ("C-M-y" . counsel-yank-pop)
         ("C-h F" . counsel-faces)       ;Overrides `Info-goto-emacs-command-node'
         ("C-h S" . counsel-info-lookup-symbol)
         ("C-c u" . counsel-unicode-char)
         ("C-c C" . counsel-colors-emacs) ;Alternative to `list-colors-display'
         ([remap execute-extended-command] . counsel-M-x)
         ([remap bookmark-jump] . counsel-bookmark) ;Jump to book or set it if it doesn't exist, C-x r b
         ([remap bookmark-set] . counsel-bookmark)  ;C-x r m
         ([remap find-file]  . counsel-find-file)
         ([remap describe-bindings] . counsel-descbinds)
         ([remap finder-by-keyword] . counsel-package) ;C-h p
         ([remap describe-variable] . counsel-describe-variable)
         ([remap describe-function] . counsel-describe-function)
         ("M-s r" . counsel-rg)
         ("M-s g" . counsel-git-grep)
         ("M-s z" . prot/counsel-fzf-rg-files)
         :map ivy-minibuffer-map
         ("C-r" . counsel-minibuffer-history)
         ("C-SPC" . ivy-restrict-to-matches))
  :config
  (setq counsel-yank-pop-preselect-last t
        counsel-yank-pop-separator "\n—————————\n"
        counsel-describe-function-function 'helpful-function
        counsel-describe-variable-function 'helpful-variable
        counsel-find-file-at-point t
        counsel-find-file-ignore-regexp
        ;; Note that `ivy-extra-directories' should also not contain the "../" and
        ;; "./" elements if you don't want to see those in the `counsel-find-file'
        ;; completion list.
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#.]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))
  (progn
    (ivy-set-actions
     'counsel-find-file
     `(("x"
        (lambda (x) (delete-file (expand-file-name x ivy--directory)))
        ,(propertize "delete" 'face 'font-lock-warning-face))))

    ;; counsel-rg
    ;; Redefine `counsel-rg-base-command' with my required options, especially
    ;; the `--follow' option to allow search through symbolic links (part of
    ;; `modi/rg-arguments').
    (setq counsel-rg-base-command
          (concat (mapconcat #'shell-quote-argument
                             (append '("rg")
                                     vde/rg-arguments
                                     '("--no-heading" ;No file names above matching content
                                       ))
                             " ")
                  " %s"            ;This MUST be %s, not %S
                                        ;https://github.com/abo-abo/swiper/issues/427
                  ))))


(use-package company
  :commands global-company-mode
  :init
  (add-hook 'after-init-hook #'global-company-mode)
  (setq
   company-idle-delay 0.2
   company-selection-wrap-around t
   company-minimum-prefix-length 2
   company-require-match nil
   company-dabbrev-ignore-case nil
   company-dabbrev-downcase nil
   company-show-numbers t
   company-tooltip-align-annotations t)
  :config
  (bind-keys :map company-active-map
             ("C-d" . company-show-doc-buffer)
             ("C-l" . company-show-location)
             ("C-n" . company-select-next)
             ("C-p" . company-select-previous)
             ("C-t" . company-select-next)
             ("C-s" . company-select-previous)
             ("TAB" . company-complete))
  (setq company-backends
        '(company-css
          company-clang
          company-capf
          company-semantic
          company-xcode
          company-cmake
          company-files
          company-gtags
          company-etags
          company-keywords)))

(use-package company-emoji
  :ensure company
  :config
  (add-to-list 'company-backends 'company-emoji))

;;; Default rg arguments
;; https://github.com/BurntSushi/ripgrep
(defconst vde/rg-arguments
  `("--no-ignore-vcs"                   ;Ignore files/dirs ONLY from `.ignore'
    "--line-number"                     ;Line numbers
    "--smart-case"
    "--max-columns" "150"      ;Emacs doesn't handle long line lengths very well
    "--ignore-file" ,(expand-file-name ".ignore" (getenv "HOME")))
  "Default rg arguments used in the functions in `counsel' and `projectile' packages.")


(if *sys/full*
    (progn
      (use-package ivy-rich
        :after ivy
        :config
        (setq ivy-virtual-abbreviate 'full
              ivy-rich-switch-buffer-align-virtual-buffer t
              ivy-rich-path-style 'abbrev)
        (ivy-rich-mode 1))

      (use-package prescient
        :config
        (setq prescient-history-length 50
              prescient-filter-method '(fuzzy initialism regexp))
        (prescient-persist-mode 1))


      (use-package ivy-prescient
        :after (prescient ivy)
        :config
        (setq ivy-prescient-sort-commands
              '(:not swiper ivy-switch-buffer counsel-switch-buffer)
              ivy-prescient-retain-classic-highlighting t
              ivy-prescient-enable-filtering t
              ivy-prescient-enable-sorting t)
        (defun prot/ivy-prescient-filters (str)
          "Specify an exception for `prescient-filter-method'.

This new rule can be used to tailor the results of individual
Ivy-powered commands, using `ivy-prescient-re-builder'."
          (let ((prescient-filter-method '(literal regexp)))
            (ivy-prescient-re-builder str)))

        (setq ivy-re-builders-alist
              '((counsel-rg . prot/ivy-prescient-filters)
                (counsel-grep . prot/ivy-prescient-filters)
                (counsel-yank-pop . prot/ivy-prescient-filters)
                (swiper . prot/ivy-prescient-filters)
                (swiper-isearch . prot/ivy-prescient-filters)
                (swiper-all . prot/ivy-prescient-filters)
                (t . ivy-prescient-re-builder)))
        (ivy-prescient-mode 1))

      (use-package company-prescient
        :ensure company
        :after (company prescient)
        :config
        (company-prescient-mode 1))

      ))

(provide 'setup-completion)
