;;; programming-config.el --- -*- lexical-binding: t -*-
;;; Commentary:
;;; Configuration files mode configuration
;;; Code:

(defconst src-dir "~/src/"
  "Where all my sources are.")
(set-register ?s `(file . ,src-dir))

(use-package yaml-mode
  :mode "\\.ya?ml\\'"
  :hook ((yaml-mode . highlight-indentation-mode)
         (yaml-mode . highlight-indentation-current-column-mode)))

(use-package conf-mode
  :mode ("\\.to?ml\\'" . conf-toml-mode))

(use-package adoc-mode
  :mode ("\\.adoc\\'" . conf-toml-mode))

(use-package flymake
  :defer t
  :bind
  (
   :map flymake-diagnostics-buffer-mode-map
   ("p" .
    (lambda()(interactive)
      (previous-line)
      (save-excursion
        (flymake-show-diagnostic(point)))))
   ("n" .
    (lambda()(interactive)
      (next-line)
      (save-excursion
        (flymake-show-diagnostic(point)))))
   (
    :map flymake-project-diagnostics-mode-map
    ("p" .
     (lambda()(interactive)
       (previous-line)
       (save-excursion
         (flymake-show-diagnostic(point)))))
    ("n" .
     (lambda()(interactive)
       (next-line)
       (save-excursion
         (flymake-show-diagnostic(point)))))))
  :hook
  (prog-mode . flyspell-prog-mode)
  (prog-mode . flymake-mode))

(use-package flymake-codespell
  :after flymake
  :hook (prog-mode . flymake-codespell-setup-backend))

;; (use-package copilot
;;   :preface
;;   (unless (package-installed-p 'copilot)
;;     (package-vc-install "https://github.com/zerolfx/copilot.el"))
;;   :hook
;;   (prog-mode . copilot-mode)
;;   ;; (markdown-mode . copilot-mode)
;;   ;; (text-mode . copilot-mode)
;;   (log-edit-mode . copilot-mode)
;;   (vc-git-log-edit-mode . copilot-mode)
;;   :bind
;;   (:map copilot-completion-map
;;         ("C-g" . copilot-clear-overlay)
;;         ("C-j" . copilot-next-completion)
;;         ("C-k" . copilot-previous-completion)
;;         ("M-RET" . copilot-accept-completion)
;;         ("C-f" . copilot-accept-completion)
;;         ("C-l" . copilot-panel-complete)
;;         ("C-<tab>" . copilot-next-completion)
;;         ("C-S-<tab>" . copilot-previous-completion))
;;   :custom
;;   (copilot-idle-delay 1))

(provide 'programming-config)
;;; programming-config.el ends here
