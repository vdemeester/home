;;; programming-nix.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Nix configuration
;;; Code:
(use-package nix-ts-mode
  :if (executable-find "nix")
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :if (executable-find "nix")
  :after nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :if (executable-find "nix")
  :after nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nixpkgs-fmt
  :if (executable-find "nix")
  :after nix-ts-mode
  ;; :custom
  ;; (nixpkgs-fmt-command "nixfmt")
  :config
  (add-hook 'nix-ts-mode-hook 'nixpkgs-fmt-on-save-mode))

(provide 'programming-nix)
;;; programming-nix.el ends here
