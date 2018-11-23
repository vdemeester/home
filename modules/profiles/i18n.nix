{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.i18n;
in
{
  options = {
    profiles.i18n = {
      enable = mkOption {
        default = true;
        description = "Enable i18n profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    i18n = {
      consoleFont = "Lat2-Terminus16";
      consoleKeyMap = "fr-bepo";
      defaultLocale = "en_US.UTF-8";
    };
  };
}
