
(require 'package)

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

(setq package-archive-priorities
      '(("melpa" .  3)
        ("org" . 2)
        ("gnu" . 1)))

(require 'tls)

;; From https://github.com/hlissner/doom-emacs/blob/5dacbb7cb1c6ac246a9ccd15e6c4290def67757c/core/core-packages.el#L102
(setq gnutls-verify-error (not (getenv "INSECURE")) ; you shouldn't use this
      tls-checktrust gnutls-verify-error
      tls-program (list "gnutls-cli --x509cafile %t -p %p %h"
                        ;; compatibility fallbacks
                        "gnutls-cli -p %p %h"
                        "openssl s_client -connect %h:%p -no_ssl2 -no_ssl3 -ign_eof"))

;; Initialise the packages, avoiding a re-initialisation.
(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil)
  (package-initialize))

(setq load-prefer-newer t)              ; Always load newer compiled files
(setq ad-redefinition-action 'accept)   ; Silence advice redefinition warnings

;; Init `delight'
(unless (package-installed-p 'delight)
  (package-refresh-contents)
  (package-install 'delight))

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(use-package orderless
  :unless noninteractive
  :config
  (setq orderless-regexp-separator " +")
  (setq orderless-matching-styles
        '(orderless-flex
	  orderless-strict-leading-initialism
          orderless-regexp
          orderless-prefixes
          orderless-literal))

  (defun prot/orderless-literal-dispatcher (pattern _index _total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))

  (defun prot/orderless-initialism-dispatcher (pattern _index _total)
    (when (string-suffix-p "," pattern)
      `(orderless-strict-leading-initialism . ,(substring pattern 0 -1))))

  (setq orderless-style-dispatchers '(prot/orderless-literal-dispatcher
                                      prot/orderless-initialism-dispatcher))
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))         ; space should never complete
(use-package marginalia
   :config
   (marginalia-mode 1)
   (setq marginalia-annotators '(marginalia-annotators-heavy
                                 marginalia-annotators-light))
   ;;:bind (:map minibuffer-local-completion-map
   ;;            ("C-i" . marginalia-cycle-annotators))
   )

(use-package minibuffer
  :config
  (setq completion-styles '(orderless partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search nil)   ; For general regexp

  ;; The following two are updated in Emacs 28.  They concern the
  ;; *Completions* buffer.
  (setq completions-format 'one-column)
  (setq completions-detailed t)

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; declarations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
  :bind (("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         :map completion-list-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)))
(use-package consult
  :config
  (setq consult-line-numbers-widen t)
  (setq consult-preview-buffer nil)
  (setq consult-preview-grep t)
  (setq consult-preview-mark t)
  (setq consult-preview-line t)
  (setq consult-preview-outline nil)
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (consult-preview-mode 1)
  :bind (("M-X" . consult-mode-command)
         ("M-s i" . consult-imenu)
         ("M-s s" . consult-outline)    ; M-s o is `occur'
         ("M-s M-s" . consult-outline)
         ("M-s m" . consult-mark)
         ("M-s l" . consult-line)
         :map minibuffer-local-completion-map
         ("<tab>" . minibuffer-force-complete)))

(use-package embark
	     :ensure t
  :after minibuffer
  :config
  (setq embark-occur-initial-view-alist '((t . zebra)))
  (setq embark-occur-minibuffer-completion t)
  (setq embark-live-occur-update-delay 0.5)
  (setq embark-live-occur-initial-delay 0.8)
  (setq embark-annotator-alist '((t . embark-annotation-function-metadatum)))
  :hook (minibuffer-setup-hook . embark-live-occur-after-input)
  :bind (("C-," . embark-act)
         :map minibuffer-local-completion-map
         ("C-," . embark-act)
         ("C-." . embark-act-noexit)
         ("M-q" . embark-occur-toggle-view) ; parallel of `fill-paragraph'
         ("M-o" . embark-export) ; falls back to `embark-occur'
         ("C-o" . embark-export)
         ("M-v" . embark-switch-to-live-occur)
         :map embark-occur-mode-map
         ("," . embark-act)
         ("M-o" . embark-export)
         ("C-o" . embark-export)
         ("M-t" . toggle-truncate-lines)
         ("M-q" . embark-occur-toggle-view)
         ;; ("M-v" . prot-minibuffer-focus-mini) ; from `prot-minibuffer.el'
         ("M-q" . embark-occur-toggle-view)))
