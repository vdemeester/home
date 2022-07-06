;;; config-editing.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Editing configuration
;;; Code:
(setq-default enable-remote-dir-locals t)

;; When finding file in non-existing directory, offer to create the
;; parent directory.
(defun with-buffer-name-prompt-and-make-subdirs ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it? " parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions #'with-buffer-name-prompt-and-make-subdirs)

;; Fix long line "problems"
;; Disable some right-to-left behavior that might not be needed.
;; Learning arabic might make me change this, but for now..
(setq-default bidi-paragraph-direction 'left-to-right)
(if (version<= "27.1" emacs-version)
    (setq bidi-inhibit-bpa t))
;; Detect if the line in a buffer are so long they could have a performance impact
(if (version<= "27.1" emacs-version)
    (global-so-long-mode 1))

(use-package saveplace
  :unless noninteractive
  :config
  (save-place-mode 1))

(use-package smartparens
  :unless noninteractive
  :commands (smartparens-mode smartparens-global-mode show-smartparens-global-mode
                              sp-split-sexp sp-newline sp-up-sexp)
  :hook ((prog-mode . turn-on-smartparens-mode)
         (markdown-mode . turn-on-smartparens-mode)
         (org-mode . turn-on-smartparens-mode)
         (prog-mode . turn-on-show-smartparens-mode)
         (markdown-mode . turn-on-show-smartparens-mode)
         (org-mode . turn-on-show-smartparens-mode)
         (emacs-lisp-mode . turn-on-smartparens-strict-mode))
  :config
  (require 'smartparens-config)

  (sp-local-pair 'minibuffer-inactive-mode "'" nil :actions nil)
  (sp-local-pair 'web-mode "{%" "%}")
  (sp-with-modes '(org-mode)
    (sp-local-pair "=" "="))
  (sp-with-modes 'emacs-lisp-mode
    ;; disable ', it's the quote character!
    (sp-local-pair "'" nil :actions nil)
    ;; also only use the pseudo-quote inside strings where it
    ;; serves as hyperlink.
    (sp-local-pair "`" "'" :when '(sp-in-string-p sp-in-comment-p))))

(use-package color-identifiers-mode
  :unless noninteractive
  :commands (color-identifiers-mode)
  :config
  (setq-default color-identifiers:num-colors 15
                color-identifiers:min-color-saturation 0.1
                color-identifiers:max-color-saturation 0.9)
  (defun myfunc-color-identifiers-mode-hook ()
    (let ((faces '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-constant-face font-lock-type-face font-lock-function-name-face font-lock-variable-name-face font-lock-keyword-face font-lock-string-face font-lock-builtin-face font-lock-preprocessor-face font-lock-warning-face font-lock-doc-face font-lock-negation-char-face font-lock-regexp-grouping-construct font-lock-regexp-grouping-backslash)))
      (dolist (face faces)
        (face-remap-add-relative face '((:foreground "" :weight normal :slant normal)))))
    (face-remap-add-relative 'font-lock-keyword-face '((:weight bold :slant normal :foreground "#666666")))
    (face-remap-add-relative 'font-lock-comment-face '((:slant italic :weight bold :foreground "#333333")))
    (face-remap-add-relative 'font-lock-comment-delimiter-face '((:slant italic :weight bold :foreground "#333333")))
    (face-remap-add-relative 'font-lock-builtin-face '((:weight bold :foreground "#666666")))
    (face-remap-add-relative 'font-lock-preprocessor-face '((:weight bold)))
    (face-remap-add-relative 'font-lock-function-name-face '((:underline t)))
    (face-remap-add-relative 'font-lock-string-face '((:weight normal :foreground "#333333")))
    (face-remap-add-relative 'font-lock-constant-face '((:foreground "#666666" :slant italic))))
  (add-hook 'color-identifiers-mode-hook 'myfunc-color-identifiers-mode-hook)
  :hook ((go-mode . color-identifiers-mode)
         (js-mode . color-identifiers-mode)
         (python-mode . color-identifiers-mode)))

(use-package aggressive-indent
  :unless noninteractive
  :bind ("C-c e i" . aggressive-indent-mode)
  :hook ((lisp-mode       . aggressive-indent-mode)
         (emacs-lisp-mode . aggressive-indent-mode))
  :config
  ;; Free C-c C-q, used in Org and in CIDER
  (unbind-key "C-c C-q" aggressive-indent-mode-map))

(use-package undo-tree
  :unless noninteractive
  :hook (after-init . global-undo-tree-mode)
  :config
  (setq-default undo-tree-visualizer-timestamps t
                undo-tree-enable-undo-in-region t))

(use-package whitespace
  :unless noninteractive
  :commands (whitespace-mode vde/toggle-invisibles)
  :config
  (setq-default whitespace-style '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark))
  (defun vde/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))
  :bind ("<f6>" . vde/toggle-invisibles))

(use-package expand-region
  :unless noninteractive
  :commands (er/expand-region er/contract-region)
  :bind (("C-=" . er/expand-region)
         ("C--". er/contract-region)))

(use-package visual-regexp
  :unless noninteractive
  :commands (vr/replace vr/query-replace)
  :bind (("C-c r"   . vr/replace)
         ("C-c %"   . vr/query-replace)))

(use-package hs-minor-mode
  :unless noninteractive
  :hook ((prog-mode . hs-minor-mode)))

(use-package easy-kill
  :unless noninteractive
  :commands (easy-kill)
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package display-line-numbers
  :unless noninteractive
  :hook (prog-mode . display-line-numbers-mode)
  :config
  (setq-default display-line-numbers-type 'relative)
  (defun vde/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode)))
  :bind ("<f7>" . vde/toggle-line-numbers))

(add-hook 'prog-mode-hook 'toggle-truncate-lines)

(use-package newcomment
  :unless noninteractive
  :config
  (setq-default comment-empty-lines t
                comment-fill-column nil
                comment-multi-line t
                comment-style 'multi-line)
  (defun prot/comment-dwim (&optional arg)
    "Alternative to `comment-dwim': offers a simple wrapper
around `comment-line' and `comment-dwim'.

If the region is active, then toggle the comment status of the
region or, if the major mode defines as much, of all the lines
implied by the region boundaries.

Else toggle the comment status of the line at point."
    (interactive "*P")
    (if (use-region-p)
        (comment-dwim arg)
      (save-excursion
        (comment-line arg))))

  :bind (("C-;" . prot/comment-dwim)
         ("C-:" . comment-kill)
         ("M-;" . comment-indent)
         ("C-x C-;" . comment-box)))

;; FIXME(vdemeester) Do I need on-the-fly spellcheck *or* not ?
(use-package flyspell
  :unless noninteractive
  :commands (flyspell-prog-mode flyspell-mode)
  :hook((text-mode . flyspell-mode)
        (prog-mode . flyspell-prog-mode))
  :config
  (define-key flyspell-mode-map (kbd "C-;") nil)
  (setq-default flyspell-issue-message-flag nil
                flyspell-issue-welcome-flag nil
                ispell-program-name "hunspell"
                ispell-local-dictionary "en_GB"
                ispell-local-dictionary-alist
                '(("en_GB"
                   "[[:alpha:]]"
                   "[^[:alpha:]]"
                   "[']"
                   nil
                   ("-d" "en_GB,fr_FR")
                   nil
                   utf-8))))

(use-package emacs
  :init
  (setq-default tab-always-indent 'complete
                tab-width 4
                indent-tabs-mode nil))

;; FIXME: enable/disable this through a minor mode
;;        can be enable by default in code, disable in adoc-mode, …
;; (use-package emacs
;; :hook (before-save . delete-trailing-whitespace))

(use-package delsel
  :unless noninteractive
  :config
  (delete-selection-mode 1))

(use-package emacs
  :unless noninteractive
  :custom
  (repeat-on-final-keystroke t)
  (set-mark-command-repeat-pop t)
  :bind ("M-z" . zap-up-to-char))

(use-package emacs
  :config
  (defun prot/new-line-below ()
    "Create a new line below the current one.  Move the point to
the absolute beginning.  Also see `prot/new-line-above'."
    (interactive)
    (end-of-line)
    (newline))

  (defun prot/new-line-above ()
    "Create a new line above the current one.  Move the point to
the absolute beginning.  Also see `prot/new-line-below'."
    (interactive)
    (beginning-of-line)
    (newline)
    (forward-line -1))

  (defun prot/yank-replace-line-or-region ()
    "Replace the line at point with the contents of the last
stretch of killed text.  If the region is active, operate over it
instead.  This command can then be followed by the standard
`yank-pop' (default is bound to M-y)."
    (interactive)
    (if (use-region-p)
        (progn
          (delete-region (region-beginning) (region-end))
          (yank))
      (progn
        (delete-region (point-at-bol) (point-at-eol))
        (yank))))

  :bind (("C-S-SPC" . contrib/mark-whole-word)
         ("<C-return>" . prot/new-line-below)
         ("<C-S-return>" . prot/new-line-above)
         ("M-SPC" . cycle-spacing)
         ("M-o" . delete-blank-lines)
         ("<C-f6>" . tear-off-window)
         ("C-S-y" . prot/yank-replace-line-or-region)))

(use-package pdf-tools
  :unless noninteractive
  :mode  ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-midnight-colors '("#ffffff" . "#000000"))
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

(use-package paste-sbr
  :unless noninteractive
  :commands (htmlize-paste-it)
  :bind ("C-c e p" . htmlize-paste-it))

(use-package scratch
  :unless noninteractive
  :commands (scratch)
  :config
  (defun vde/scratch-buffer-setup ()
    "Add contents to `scratch' buffer and name it accordingly.
If region is active, add its contents to the new buffer."
    (let* ((mode major-mode)
           (string (format "Scratch buffer for: %s\n\n" mode))
           (region (with-current-buffer (current-buffer)
                     (if (region-active-p)
                         (buffer-substring-no-properties
                          (region-beginning)
                          (region-end)))
                     ""))
           (text (concat string region)))
      (when scratch-buffer
        (save-excursion
          (insert text)
          (goto-char (point-min))
          (comment-region (point-at-bol) (point-at-eol)))
        (forward-line 2))
      (rename-buffer (format "*Scratch for %s*" mode) t)))
  :hook (scratch-create-buffer . vde/scratch-buffer-setup)
  :bind ("C-c s" . scratch))

(use-package subword
  :diminish
  :hook (prog-mode-hook . subword-mode))

(provide 'config-editing)
;;; config-editing.el ends here
