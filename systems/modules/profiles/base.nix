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
      systemd-boot = mkOption {
        default = true;
        description = "Enable systemd-boot for boot loading";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    warnings = [ "The option 'profiles.base.boot' is deprecated, use 'core.boot.systemd-boot' instead" ];
    boot.loader.systemd-boot.enable = cfg.systemd-boot;
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
        eza
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
