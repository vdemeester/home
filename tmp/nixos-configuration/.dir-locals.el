;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil (eval . (setq projectile-project-compilation-cmd "nixos-rebuild dry-build"
		    projectile-project-run-cmd "nixos-rebuild switch")))
 (nix-mode
  (tab-width . 2)))
