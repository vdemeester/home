(use-package marginalia
  :ensure t
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode 1))
(use-package minibuffer
  :hook (after-init . minibuffer-depth-indicate-mode) ; recursion depth
  :config
  (setq enable-recursive-minibuffers t)
  (setq completion-styles '(partial-completion flex))
  (setq completion-auto-help nil)
  (setq completion-flex-nospace nil)
  (setq read-file-name-completion-ignore-case t
        read-buffer-completion-ignore-case t)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  ;; (setq resize-mini-windows t)
  (setq completion-show-help nil))
(use-package icomplete
  :hook (after-init . icomplete-mode)
  :custom
  (icomplete-in-buffer t)
  (icomplete-delay-completions-threshold 0)
  :config
  :bind (:map icomplete-minibuffer-map
              ("<return>" . icomplete-force-complete-and-exit)              
              ("C-s" . icomplete-forward-completions)
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)))
(use-package icomplete-vertical
  :after icomplete
  :hook (icomplete-mode . icomplete-vertical-mode))
(defgroup prot-orderless ()
  "Tweaks for the Orderless completion style."
  :group 'minibuffer)

(defcustom prot-orderless-default-styles
  '(orderless-flex
    orderless-strict-leading-initialism
    orderless-regexp
    orderless-prefixes
    orderless-literal)
  "List that should be assigned to `orderless-matching-styles'."
  :type 'list
  :group 'prot-orderless)

(defcustom prot-orderless-alternative-styles
  '(orderless-literal
    orderless-prefixes
    orderless-strict-leading-initialism
    orderless-regexp)
  "Alternative list for `orderless-matching-styles'.

Unlike `prot-orderless-default-styles', this variable is intended
for use on a case-by-case basis, with the help of the function
`prot-orderless-with-styles'."
  :type 'list
  :group 'prot-orderless)

(defun prot-orderless-literal-dispatcher (pattern _index _total)
  "Literal style dispatcher using the equals sign as a suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))

(defun prot-orderless-initialism-dispatcher (pattern _index _total)
  "Leading initialism  dispatcher using the comma suffix.
It matches PATTERN _INDEX and _TOTAL according to how Orderless
parses its input."
  (when (string-suffix-p "," pattern)
    `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

(defvar orderless-matching-styles)

(defun prot-orderless-with-styles (cmd &optional styles)
  "Call CMD with optional orderless STYLES.

STYLES is a list of pattern matching methods that is passed to
`orderless-matching-styles'.  Its fallback value is that of
`prot-orderless-alternative-styles'."
  (let ((orderless-matching-styles (or styles prot-orderless-alternative-styles))
        (this-command cmd))
    (call-interactively cmd)))

(use-package orderless
  :config
    (setq prot-orderless-default-styles
        '(orderless-prefixes
          orderless-literal
          orderless-strict-leading-initialism
          orderless-regexp
          orderless-flex))
  (setq prot-orderless-alternative-styles
        '(orderless-literal
          orderless-prefixes
          orderless-strict-leading-initialism
          orderless-regexp))
  (setq orderless-component-separator " +")
  (setq orderless-matching-styles prot-orderless-default-styles)
  (setq orderless-style-dispatchers
        '(prot-orderless-literal-dispatcher
          prot-orderless-initialism-dispatcher))
  ;; SPC should never complete: use it for `orderless' groups.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))

(setq completion-styles
            '(partial-completion basic flex initials substring)
            completion-category-overrides
            '((file (styles basic flex initials substring))
              (buffer (styles basic flex initials substring))
              (info-menu (styles basic flex initials substring))))
