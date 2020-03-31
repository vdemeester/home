;;; -*- lexical-binding: t; -*-
(use-package nix-mode
  :mode ("\\.nix\\'" "\\.nix.in\\'"))

(use-package nix-drv-mode
  :ensure nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :ensure nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nixpkgs-fmt
  :after nix-mode
  :config
  (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode))

(provide 'setup-nix)
