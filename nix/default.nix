let
  sources = import ./sources.nix;
in
rec {
  home-manager = import (sources.home-manager + "/nixos");
  home-manager-stable = import (sources.home-manager-stable + "/nixos");
  lib = import (sources.nixos + "/lib");
  pkgs = import sources.nixos;
  lib-unstable = import (sources.nixos-unstable + "/lib");
  pkgs-unstable = import sources.nixos-unstable;
  nixpkgs = import sources.nixpkgs;
  emacs = import sources.emacs-overlay;
  gitignore = import sources.gitignore;
  nixos-hardware = import sources.nixos-hardware;
  sops-nix = import sources.sops-nix;
}
