;;; programming-nix.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Nix configuration
;;; Code:
(use-package nix-mode
  :if *nix*
  :mode ("\\.nix\\'" "\\.nix.in\\'")
  :config
  (add-to-list 'lsp-language-id-configuration '(nix-mode . "nix"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection '("rnix-lsp"))
                    :major-modes '(nix-mode)
                    :server-id 'nix)))

(use-package nix-drv-mode
  :if *nix*
  :after nix-mode
  :mode "\\.drv\\'")

(use-package nix-shell
  :if *nix*
  :after nix-mode
  :commands (nix-shell-unpack nix-shell-configure nix-shell-build))

(use-package nixpkgs-fmt
  :if *nix*
  :after nix-mode
  :config
  (add-hook 'nix-mode-hook 'nixpkgs-fmt-on-save-mode))

(provide 'programming-nix)
;;; programming-nix.el ends here
