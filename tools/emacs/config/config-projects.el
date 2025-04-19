;;; config-projects.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Project related configuration.
;;; Code:

(require 'json)

(use-package project
  :commands (project-find-file project-find-regexp vde/project-vterm vde/project-run-in-vterm)
  :custom ((project-switch-commands '((?f "File" project-find-file)
				      (?g "Grep" project-find-regexp)
				      (?d "Dired" project-dired)
				      (?b "Buffer" project-switch-to-buffer)
				      (?q "Query replace" project-query-replace-regexp)
				      (?m "Magit" vde-project-magit-status)
				      (?e "Eshell" project-eshell)
				      (?E "Eat" vde/project-eat)
				      (?s "Vterm" vde/project-vterm)
				      (?R "README" vde/open-readme)
				      (?g "Checkout GitHub PR" checkout-github-pr)))
	   (project-mode-line t))
  :bind (("C-x p v" . vde-project-magit-status)
         ("C-x p s" . vde/project-vterm)
         ("C-x p X" . vde/project-run-in-vterm)
	 ("C-x p E" . vde/project-eat)
	 ("C-x p G" . checkout-github-pr))
  :init
  ;; (require project-rootfile)
  ;; (add-to-list 'project-find-functions #'project-rootfile-try-detect t)
  ;; (setq project-rootfile-list '(".project"
  ;;   "default.nix" "flake.nix"                               ; nix
  ;;   "Makefile" "GNUMakefile" "CMakeLists.txt"               ; Make & CMake
  ;;   "Cask" "Eldev" "Keg" "Eask"                             ; Emacs
  ;;   "stack.yaml"                                            ; Haskell
  ;;   "Cargo.toml"                                            ; Rust
  ;;   "go.mod"                                                ; Go
  ;;   ))
  :config
  (setq vde/project-local-identifier '(".project")) ;; "go.mod"

  ;; (add-hook 'project-find-functions #'vde/project-try-local)

  (setq-default project-compilation-buffer-name-function 'project-prefixed-buffer-name)
  (defun vde-project-magit-status ()
    "Run `magit-status' on project."
    (interactive)
    (magit-status (vde-project--project-current)))

  ;; (general-leader
  ;;   "p"  '(:ignore :which-key "Project")
  ;;   "pp"  #'(project-switch-project :which-key "Switch to Project")
  ;;   "ps"  #'(project-search :which-key "Grep in Project")
  ;;   "pf"  #'(project-find-file :which-key "Find in Project")
  ;;   "pd"  #'(project-dired :which-key "Dired in Project")
  ;;   "pc"  #'(project-compile :which-key "Compile in Project")
  ;;   "pb"  #'(project-switch-to-buffer :which-key "Switch to Project Buffer")
  ;;   "pk"  #'(project-kill-buffers :which-key "Kill Project Buffers")
  ;;   "ps"  #'(vde/project-vterm :which-key "Start a vterm in Project")
  ;;   "pe"  #'(project-eshell :which-key "Start a eshell in Project")
  ;;   "pE"  #'(vde/project-eat :which-key "Start a eat term in Project")
  ;;   "px"  #'(vde/project-run-in-vterm :which-key "Execute command in vterm in Project"))
  )

(use-package conner
  :bind (("C-x p C" . conner-run-project-command))
  :commands (conner-run-project-command)
  :config
  (require 'vterm))

(use-package project-x
  :after project
  :config
  (add-hook 'project-find-functions 'project-x-try-local 90)
  (add-hook 'kill-emacs-hook 'project-x--window-state-write)
  (add-to-list 'project-switch-commands
               '(?j "Restore windows" project-x-windows) t)
  :bind (("C-x p w" . project-x-window-state-save)
         ("C-x p j" . project-x-window-state-load)))

(provide 'config-projects)
;;; config-projects.el ends here
