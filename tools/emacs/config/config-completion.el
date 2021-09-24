;;; config-completion.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Setup completion framework
;;; Code

(use-package orderless
  :unless noninteractive
  :config
  (setq orderless-regexp-separator " +")
  (setq orderless-matching-styles
        '(orderless-prefixes
          orderless-literal
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-flex))

  (defun vde/orderless-literal-dispatcher (pattern _index _total)
    "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun vde/orderless-initialism-dispatcher (pattern _index _total)
    "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
    (when (string-suffix-p "," pattern)
      `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

  (setq orderless-style-dispatchers '(vde/orderless-literal-dispatcher
                                      vde/orderless-initialism-dispatcher))
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))         ; space should never complete

(use-package marginalia
  :config
  (setq marginalia-max-relative-age 0)  ; time is absolute here!
  (setq marginalia-annotators '(marginalia-annotators-heavy
                                marginalia-annotators-light))
  (marginalia-mode 1))

(use-package minibuffer
  :unless noninteractive
  :config
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-ignore-case t)
  (setq-default case-fold-search nil)   ; For general regexp
  ;; NOTE: flex completion is introduced in Emacs 27
  (setq completion-show-help nil)
  (setq completion-styles
        '(substring initials flex partial-completion orderless))
  (setq completion-category-overrides
        '((file (styles . (partial-completion orderless)))))
  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.
  (setq completions-format 'one-column)
  (setq completions-detailed t)

  ;; Grouping of completions for Emacs 28
  (setq completions-group t)
  (setq completions-group-sort nil)
  (setq completions-group-format
        (concat
         (propertize "    " 'face 'completions-group-separator)
         (propertize " %s " 'face 'completions-group-title)
         (propertize " " 'face 'completions-group-separator
                     'display '(space :align-to right))))

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  (defun vde/focus-minibuffer ()
    "Focus the active minibuffer.

Bind this to `completion-list-mode-map' to M-v to easily jump
between the list of candidates present in the \\*Completions\\*
buffer and the minibuffer (because by default M-v switches to the
completions if invoked from inside the minibuffer."
    (interactive)
    (let ((mini (active-minibuffer-window)))
      (when mini
        (select-window mini))))

  (defun vde/focus-minibuffer-or-completions ()
    "Focus the active minibuffer or the \\*Completions\\*.

If both the minibuffer and the Completions are present, this
command will first move per invocation to the former, then the
latter, and then continue to switch between the two.

The continuous switch is essentially the same as running
`vde/focus-minibuffer' and `switch-to-completions' in
succession."
    (interactive)
    (let* ((mini (active-minibuffer-window))
           (completions (get-buffer-window "*Completions*")))
      (cond ((and mini
                  (not (minibufferp)))
             (select-window mini nil))
            ((and completions
                  (not (eq (selected-window)
                           completions)))
             (select-window completions nil)))))

  ;; Technically, this is not specific to the minibuffer, but I define
  ;; it here so that you can see how it is also used from inside the
  ;; "Completions" buffer
  (defun vde/describe-symbol-at-point (&optional arg)
    "Get help (documentation) for the symbol at point.

With a prefix argument, switch to the *Help* window.  If that is
already focused, switch to the most recently used window
instead."
    (interactive "P")
    (let ((symbol (symbol-at-point)))
      (when symbol
        (describe-symbol symbol)))
    (when current-prefix-arg
      (let ((help (get-buffer-window "*Help*")))
        (when help
          (if (not (eq (selected-window) help))
              (select-window help)
            (select-window (get-mru-window)))))))

  ;; Defines, among others, aliases for common actions to Super-KEY.
  ;; Normally these should go in individual package declarations, but
  ;; their grouping here makes things easier to understand.
  :bind (("M-v" . vde/focus-minibuffer-or-completions)
         :map completion-list-mode-map
         ("h" . vde/describe-symbol-at-point)
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)
         ("M-v" . vde/focus-minibuffer)))

(use-package vertico
  :unless noninteractive
  :config
  (vertico-mode))

(use-package embark
  :unless noninteractive
  :bind (("C-." . embark-act)))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode))

(use-package consult
  :unless noninteractive
  :after minibuffer
  :init
  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (setq consult-narrow-key "<") ;; (kbd "C-+")
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep consult-bookmark consult-xref
   consult--source-file consult--source-project-file consult--source-bookmark
   :preview-key (kbd "M-."))
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-project-root-function
        (lambda ()
          (when-let (project (project-current))
            (car (project-roots project)))))

  :bind (("M-X" . consult-mode-command)
         ("C-c b" . consult-bookmark)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g m" . consult-mark)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g I" . consult-imenu-multi)
         ;; "live" search
         ("M-s s g" . consult-grep)
         ("M-s s G" . consult-gip-grep)
         ("M-s s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch)
         :map minibuffer-local-completion-map
         ("<tab>" . minibuffer-force-complete)
         :map isearch-mode-map
         ("M-e" . consult-isearch)                 ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch)               ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi)))

(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

(use-package company
  :unless noninteractive
  :hook ((prog-mode . company-mode))
  :commands (global-company-mode company-mode company-indent-or-complete-common)
  :bind (("M-/" . hippie-expand)
         :map company-active-map
         ("C-d" . company-show-doc-buffer)
         ("C-l" . company-show-location)
         ("C-t" . company-select-next)
         ("C-s" . company-select-previous)
         ("C-<up>" . company-select-next)
         ("C-<down>" . company-select-previous)
         ("C-r" . company-complete-selection)
         ("TAB" . company-complete-common-or-cycle))
  :config
  (defun company-complete-common-or-selected ()
    "Insert the common part, or if none, complete using selection."
    (interactive)
    (when (company-manual-begin)
      (if (not (equal company-common company-prefix))
          (company--insert-candidate company-common)
        (company-complete-selection))))
  (setq-default company-idle-delay 0.2
                company-selection-wrap-around t
                company-minimum-prefix-length 2
                company-require-match nil
                company-dabbrev-ignore-case nil
                company-dabbrev-downcase nil
                company-show-numbers t
                company-tooltip-align-annotations t)
  (setq company-backends
        '(company-capf
          company-files
          (company-dabbrev-code
           company-gtags
           company-etags)
          company-dabbrev
          company-keywords))

  ;; We don't want completion to prevent us from actually navigating the code
  (define-key company-active-map (kbd "<return>") nil)
  (define-key company-active-map (kbd "RET") nil)
  (define-key company-active-map (kbd "C-p") nil)
  (define-key company-active-map (kbd "C-n") nil))

;; FIXME(vdemeester) Do this in programming-*.el
;; Add company-css to css-mode company-backends
;; (setq-local company-backends (append '(company-css) company-backends))
;; Same for clang, cmake or xcode, elisp

(use-package company-emoji
  :unless noninteractive
  :hook ((markdown-mode . my-company-emoji))
  :config
  (defun my-company-emoji ()
    (set (make-local-variable 'company-backends) '(company-emoji))
    (company-mode t)))

(provide 'config-completion)
;;; config-completion.el ends here
