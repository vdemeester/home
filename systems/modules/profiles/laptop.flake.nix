{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkForce;
  cfg = config.profiles.laptop;
in
{
  options = {
    profiles.laptop = {
      enable = mkEnableOption "laptop configuration";
    };
  };
  config = mkIf cfg.enable {
    modules.desktop.enable = true;
    nix = {
      sshServe.enable = mkForce false;
    };
  };
}
