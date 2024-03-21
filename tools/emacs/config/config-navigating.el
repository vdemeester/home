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

  :hook ((occur-mode . hl-line-mode)
         (occur-mode . (lambda ()
                         (toggle-truncate-lines t))))
  :bind (("M-s M-o" . multi-occur)
         :map occur-mode-map
         ("t" . toggle-truncate-lines)))

(use-package avy
  :unless noninteractive
  :commands (avy-goto-char avy-goto-line avy-goto-word-1 avy-pop-mark avy-goto-char-timer)
  :bind (("C-c j w" . avy-goto-word-1)
         ("C-c j b" . avy-pop-mark)
         ("C-c j t" . avy-goto-char-timer)
         ("C-c j l" . avy-goto-line)
	 (:map isearch-mode-map ("C-j" . avy-isearch))))

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

  :hook ((imenu-after-jump . (lambda ()
                               (when (and (eq major-mode 'org-mode)
                                          (org-at-heading-p))
                                 (org-show-entry)
                                 (org-reveal t))))))

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

(use-package bookmark+)

(keymap-global-set "S-<down-mouse-2>" 'strokes-do-stroke)

(use-package repeat
  :config
  (setq repeat-on-final-keystroke t)
  (setq set-mark-command-repeat-pop t)

  (repeat-mode 1)

  (defvar isearch-repeat-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s") #'isearch-repeat-forward)
    (define-key map (kbd "r") #'isearch-repeat-backward)
    map))

  (dolist (cmd '(isearch-repeat-forward isearch-repeat-backward))
    (put cmd 'repeat-map 'isearch-repeat-map))

  (defvar buffer-navigation-map
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "n") #'next-line)
      (define-key map (kbd "p") #'previous-line)
      (define-key map (kbd "f") #'forward-word)
      (define-key map (kbd "b") #'backward-word)
      (define-key map (kbd "u") #'scroll-up-command)
      (define-key map (kbd "d") #'scroll-down-command)
      map))

  (dolist (cmd '(next-line previous-line forward-word backward-word scroll-up-command scroll-down-command))
    (put cmd 'repeat-map 'buffer-navigation-map)))

(provide 'config-navigating)
;;; config-navigating.el ends here
