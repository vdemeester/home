{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkDefault mkForce;
  cfg = config.modules.desktop.wayland;
in
{
  options = {
    modules.desktop.wayland = {
      enable = mkEnableOption "Enable wayland desktop";
    };
  };
  config = mkIf cfg.enable {
    # Enable desktop module if not already.
    modules.desktop.enable = true;
    # Force disable xorg desktop module
    modules.desktop.xorg.enable = mkForce false;
    # Hardware Support for Wayland Sway, …
    hardware = {
      opengl = {
        enable = true;
        driSupport = true;
      };
    };
    environment.systemPackages = with pkgs; [
      qogir-icon-theme
    ];
  };
}
