{ lib, pkgs, ... }:
let
  dummyConfig = pkgs.writeText "configuration.nix" ''
    # assert builtins.trace "This is a dummy config, use switch!" false;
    {}
  '';
in
{
  imports = [
    (import ../nix).home-manager
    ../modules/module-list.nixos.nix
    # hardware
    ../hardware/thinkpad-x220.nix
    # FIXME: remove this
    ../machines/home.nixos.nix
  ];

  networking = {
    hostName = "wakasu";
  };

  # FIXME move this away
  home-manager.users.vincent = import ../home.nix;
  home-manager.users.root = { pkgs, ... }: {
    home.packages = with pkgs; [ htop ];
  };

  # FIXME: move this away
  profiles.nix-config.enable = false;
  home-manager.useGlobalPkgs = true;
  nix.nixPath = [
    "nixos-config=${dummyConfig}"
    "nixpkgs=/run/current-system/nixpkgs"
    "nixpkgs-overlays=/run/current-system/overlays/compat"
  ];

  nixpkgs = {
    overlays = [
      (import ../overlays/sbr.nix)
      (import ../overlays/unstable.nix)
      (import ../nix).emacs
    ];
    config = {
      allowUnfree = true;
      packageOverrides = pkgs: {
        nur = import (builtins.fetchTarball "https://github.com/nix-community/NUR/archive/master.tar.gz") {
          inherit pkgs;
        };
      };
    };
  };

  # FIXME: put this in a common
  system = {
    extraSystemBuilderCmds = ''
      ln -sv ${pkgs.path} $out/nixpkgs
      ln -sv ${../overlays} $out/overlays
    '';

    stateVersion = "20.03";
  };
}
