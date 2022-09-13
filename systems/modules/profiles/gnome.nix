{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop.gnome;
in
{
  options = {
    profiles.desktop.gnome = {
      enable = mkEnableOption "Enable Gnome desktop profile";
    };
  };
  config = mkIf cfg.enable {
    modules.services.avahi.enable = true;
    profiles = {
      desktop.enable = true;
    };
    services = {
      gnome3 = {
        chrome-gnome-shell.enable = true;
        core-shell.enable = true;
        core-os-services.enable = true;
        core-utilities.enable = true;
      };
      xserver = {
        displayManager.gdm.enable = true;
        desktopManager.gnome3.enable = true;
      };
    };
  };
}
