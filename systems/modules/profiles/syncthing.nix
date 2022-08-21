{ config, lib, pkgs, ... }:

with lib;
{
  options = {
    profiles.syncthing = {
      enable = mkEnableOption "Enable syncthing profile";
    };
  };
  config = mkIf cfg.enable {
    warnings = [ "The option 'profiles.syncthing' is deprecated, use 'modules.desktop.syncthing' instead" ];
    modules.desktop.syncthing = cfg.enable;
  };
}
