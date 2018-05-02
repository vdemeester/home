{ config, pkgs, ... }:

{
  nixpkgs = {
    overlays = [
      (import ../overlays/sbr.overlay.nix)
      (import ../overlays/sbr.emacs.nix)
      # add third-party packages from outside the nixpkgs tree
      (self: super: {
        nix-beautify = import ../pkgs/nix-beautify { inherit pkgs; };
        home-manager = import ../pkgs/home-manager { inherit pkgs; };
      })
      ];
  };
}
