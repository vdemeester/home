{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.base;
in
{
  options = {
    profiles.base = {
      enable = mkOption {
        default = true;
        description = "Enable base profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    environment.pathsToLink = [
      "/share/nix-direnv"
    ];
    environment = {
      variables = {
        EDITOR = pkgs.lib.mkOverride 0 "vim";
      };
      systemPackages = with pkgs; [
        cachix
        direnv
        # exa # TODO: switch to eza in 2024
        file
        htop
        iotop
        lsof
        netcat
        psmisc
        pv
        tmux
        tree
        vim
        vrsync
        wget
        gnumake
      ];
    };
  };
}
