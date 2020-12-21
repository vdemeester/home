;;; config-navigating.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Navigation related configuration
;;; Code:

(use-package emacs
  :config
  (setq-default scroll-preserve-screen-position t)
  (setq-default scroll-conservatively 1) ; affects `scroll-step'
  (setq-default scroll-margin 0)

  (define-minor-mode vde/scroll-centre-cursor-mode
    "Toggle centred cursor scrolling behaviour."
    :init-value nil
    :lighter " S="
    :global nil
    (if vde/scroll-centre-cursor-mode
        (setq-local scroll-margin (* (frame-height) 2)
                    scroll-conservatively 0
                    maximum-scroll-margin 0.5)
      (dolist (local '(scroll-preserve-screen-position
                       scroll-conservatively
                       maximum-scroll-margin
                       scroll-margin))
        (kill-local-variable `,local))))

  ;; C-c l is used for `org-store-link'.  The mnemonic for this is to
  ;; focus the Line and also works as a variant of C-l.
  :bind ("C-c L" . vde/scroll-centre-cursor-mode))

(use-package replace
  :config
  (setq list-matching-lines-jump-to-current-line t)
  ;; See my "Modus themes" for these inherited faces
  (setq list-matching-lines-buffer-name-face
        '(:inherit modus-theme-intense-neutral :weight bold))
  (setq list-matching-lines-current-line-face
        '(:inherit modus-theme-special-mild))

  (defun vde/occur-url ()
    "Produce list with all URLs in the current buffer."
    (interactive)
    (let ((urls browse-url-button-regexp))
      (occur urls "\\&")))

  (defun vde/occur-browse-url-in-buffer ()
    "Run `eww' on a URL from the buffer using completion.
Also see `vde/occur-url'."
    (interactive)
    (let ((matches nil))
      (save-excursion
        (goto-char (point-min))
        (while (search-forward-regexp browse-url-button-regexp nil t)
          (push (match-string-no-properties 0) matches)))
      (icomplete-vertical-do
          (:height (/ (frame-height) 4) :separator 'dotted-line)
        (eww
         (completing-read "Browse URL: " matches nil t)))))

  (defun vde/occur-visit-or-list-urls (&optional arg)
    "Wrap `vde/occur-visit-or-list-urls' and `vde/occur-url'.
Meant to economise on key bindings."
    (interactive "P")
    (if arg
        (vde/occur-url)
      (vde/occur-browse-url-in-buffer)))

  :hook ((occur-mode . hl-line-mode)
         (occur-mode . (lambda ()
                         (toggle-truncate-lines t))))
  :bind (("M-s u" . vde/occur-visit-or-list-urls)
         ("M-s M-o" . multi-occur)
         :map occur-mode-map
         ("t" . toggle-truncate-lines)))

(use-package avy
  :unless noninteractive
  :commands (avy-goto-char avy-goto-line avy-goto-word-1 avy-pop-mark avy-goto-char-timer)
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j b" . avy-pop-mark)
         ("C-c j t" . avy-goto-char-timer)
         ("C-c j l" . avy-goto-line)))

(use-package hideshow
  :unless noninteractive
  :commands (hs-show-all hs-toggle-hiding hs-hide-all hs-hide-block hs-hide-level)
  :defer 5
  :bind (("C-c @ a" . hs-show-all)
         ("C-c @ c" . hs-toggle-hiding)
         ("C-c @ t" . hs-hide-all)
         ("C-c @ d" . hs-hide-block)
         ("C-c @ l" . hs-hide-level)))

(use-package mwim
  :unless noninteractive
  :commands (mwim-beginning-of-code-or-line mwim-end-of-code-or-line)
  :bind (:map prog-mode-map
              ("C-a" . mwim-beginning-of-code-or-line)
              ("C-e" . mwim-end-of-code-or-line)))

(use-package beginend
  :unless noninteractive
  :defer 5
  :config
  (beginend-global-mode 1))

(use-package dumb-jump
  :unless noninteractive
  :bind (("M-g q" . dumb-jump-quick-look) ;; Show me in a tooltip.
         ("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g ." . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g p" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window)
         ("M-g a" . xref-find-apropos)) ;; aka C-M-.
  :config
  ;; If source file is visible, just shift focus to it.
  (setq-default dumb-jump-use-visible-window t
                dumb-jump-prefer-searcher 'rg))

(use-package imenu
  :unless noninteractive
  :config
  (setq-default imenu-use-markers t
                imenu-auto-rescan t
                imenu-auto-rescan-maxout 600000
                imenu-max-item-length 100
                imenu-use-popup-menu nil
                imenu-eager-completion-buffer t
                imenu-space-replacement " "
                imenu-level-separator "/")

  (defun prot/imenu-vertical ()
    "Use a vertical Icomplete layout for `imenu'.
Also configure the value of `orderless-matching-styles' to avoid
aggressive fuzzy-style matching for this particular command."
    (interactive)
    (let ((orderless-matching-styles    ; make sure to check `orderless'
           '(orderless-literal
             orderless-regexp
             orderless-prefixes)))
      (icomplete-vertical-do (:height (/ (frame-height) 4))
        (call-interactively 'imenu))))

  :hook ((imenu-after-jump . (lambda ()
                               (when (and (eq major-mode 'org-mode)
                                          (org-at-heading-p))
                                 (org-show-entry)
                                 (org-reveal t)))))
  :bind ("C-'" . prot/imenu-vertical))

(use-package flimenu
  :unless noninteractive
  :config
  (flimenu-global-mode 1))

(use-package man
  :unless noninteractive
  :commands (man)
  :bind (:map Man-mode-map
              ("i" . Man-goto-section)
              ("g" . Man-update-manpage)))

(use-package pulse
  :unless noninteractive
  :config
  (defface vde/pulse-line-modus-theme
    '((t :inherit modus-theme-subtle-green :extend t))
    "Ad-hoc face for `vde/pulse-line'.
This is done because it is not possible to highlight empty lines
without the `:extend' property.")

  (defun vde/pulse-line (&optional face)
    "Temporarily highlight the current line."
    (interactive)
    (let ((start (if (eobp)
                     (line-beginning-position 0)
                   (line-beginning-position)))
          (end (line-beginning-position 2))
          (pulse-delay .04)
          (face
           (if face
               face
             'vde/pulse-line-modus-theme)))
      (pulse-momentary-highlight-region start end face)))
  (defun ct/yank-pulse-advice (orig-fn &rest args)
    ;; Define the variables first
    (let (begin end)
      ;; Initialize `begin` to the current point before pasting
      (setq begin (point))
      ;; Forward to the decorated function (i.e. `yank`)
      (apply orig-fn args)
      ;; Initialize `end` to the current point after pasting
      (setq end (point))
      ;; Pulse to highlight!
      (pulse-momentary-highlight-region begin end)))
  (advice-add 'yank :around #'ct/yank-pulse-advice)
  :bind ("<C-escape>" . vde/pulse-line))

(provide 'config-navigating)
;;; config-navigating.el ends here
