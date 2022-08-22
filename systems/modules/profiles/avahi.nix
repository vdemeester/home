{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.avahi;
in
{
  options = {
    profiles.avahi = {
      enable = mkEnableOption "Enable avahi profile";
    };
  };
  config = mkIf cfg.enable {
    warnings = [ "The option 'profiles.ahavi' is deprecated, use 'modules.services.avahi' instead." ];
    modules.services.avahi.enable = true;
  };
}
