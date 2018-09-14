{ config, pkgs, ... }:

{
  nixpkgs = {
    overlays = [
      (import ../overlays/sbr.overlay.nix)
      ];
  };
}
