;;; config-completion.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Setup completion framework
;;; Code

;; https://github.com/oantolin/embark/blob/master/embark-consult.el
(use-package embark
  :unless noninteractive
  :bind
  ("C-." . embark-act)
  ("M-." . embark-dwim)
  ("C-h b" . embark-bindings)
  ("C-h B" . embark-bindings-at-point)
  ("C-h M" . embark-bindings-in-keymap)
  ("C-h E" . embark-on-last-message)
  (:map completion-list-mode-map
        ("." . embark-act))
  (:map embark-collect-mode-map
        ("a") ; I don't like my own default :)
        ("." . embark-act)
        ("F" . consult-focus-lines))
  (:map embark-package-map
        ("t" . try))
  (:map embark-identifier-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map))
  (:map embark-expression-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map))
  (:map embark-region-map
        ("(" . insert-parentheses)
        ("[" . insert-pair-map)
        ("D" . dictionary-search))
  (:map embark-email-map
        ("+" . add-email-to-ecomplete)
        ("\\" . remove-email-from-ecomplete))
  (:map embark-encode-map
        ("p" . topaz-paste-region))
  (:map embark-url-map
        ("x" . browse-url-generic)
        ("p" . pocket-lib-add-urls))
  (:map embark-identifier-map
        ("D" . dictionary-lookup-definition))
  :custom
  (embark-quit-after-action nil)
  (prefix-help-command #'embark-prefix-help-command)
  (embark-indicators '(embark-minimal-indicator
                       embark-highlight-indicator
                       embark-isearch-highlight-indicator))
  (embark-cycle-key ".")
  (embark-help-key "?")
  (embark-confirm-act-all nil)
  :config
  (setq embark-candidate-collectors
        (cl-substitute 'embark-sorted-minibuffer-candidates
                       'embark-minibuffer-candidates
                       embark-candidate-collectors))
  (dolist (cmd '(comment-dwim
                 insert-parentheses
                 insert-pair
                 markdown-insert-code
                 markdown-insert-italic
                 markdown-insert-bold
                 org-emphasize
                 cdlatex-math-modify
                 TeX-font))
    (push #'embark--mark-target (alist-get cmd embark-around-action-hooks)))
  (push #'embark--xref-push-marker
        (alist-get 'find-file embark-pre-action-hooks))
  (defun embark-on-last-message (arg)
    "Act on the last message displayed in the echo area."
    (interactive "P")
    (with-current-buffer "*Messages*"
      (goto-char (1- (point-max)))
      (embark-act arg))))

(use-package embark-consult
  :unless noninteractive)

(use-package mct
  :unless noninteractive
  :custom
  (mct-completion-window-size (cons #'mct-frame-height-third 1))
  (mct-remove-shadowed-file-names t) ; works when `file-name-shadow-mode' is enabled
  (mct-hide-completion-mode-line t)
  (mct-minimum-input 3)
  (mct-live-completion t)
  (mct-live-update-delay 0.6)
  (mct-persist-dynamic-completion t)
  :config
  (mct-mode 1))

(use-package marginalia
  :unless noninteractive
  :config
  (marginalia-mode 1))

(use-package corfu
  :unless noninteractive
  :config
  (global-corfu-mode 1)
  (corfu-popupinfo-mode 1)
    ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history))

  ;; Adapted from Corfu's manual.
  (defun contrib/corfu-enable-always-in-minibuffer ()
    "Enable Corfu in the minibuffer if MCT or Vertico is not active.
Useful for prompts such as `eval-expression' and `shell-command'."
    (unless (or (bound-and-true-p vertico--input)
                (bound-and-true-p mct--active))
      (corfu-mode 1)))

  (add-hook 'minibuffer-setup-hook #'contrib/corfu-enable-always-in-minibuffer 1))

(provide 'config-completion)
;;; config-completion.el ends here
