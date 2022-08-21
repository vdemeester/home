{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.syncthing;
in
{
  options = {
    profiles.syncthing = {
      enable = mkEnableOption "Enable syncthing profile";
    };
  };
  config = mkIf cfg.enable {
    warnings = [ "The option 'profiles.syncthing' is deprecated, use 'modules.desktop.syncthing' instead" ];
    modules.services.syncthing.enable = cfg.enable;
  };
}
