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
      xserver.enable = true;
    };
  };
}
