{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop.i3;
in
{
  options = {
    profiles.desktop.i3 = {
      enable = mkEnableOption "Enable i3 desktop profile";
    };
  };

  config = mkIf cfg.enable {
    profiles = {
      desktop.enable = true;
      avahi.enable = true;
    };
    services = {
      blueman.enable = true;
      autorandr.enable = true;
      xserver = {
        enable = true;
        displayManager = {
          defaultSession = "none+i3";
          lightdm.enable = true;
          lightdm.greeters.mini.enable = true;
          lightdm.greeters.mini.user = "vincent";
        };
        windowManager.i3.enable = true;
      };
      dbus = {
        enable = true;
        # socketActivated = true;
        packages = [ pkgs.gnome3.dconf ];
      };
    };
  };
}
