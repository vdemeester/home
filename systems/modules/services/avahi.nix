{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.modules.services.avahi;
in
{
  options = {
    modules.services.avahi = {
      enable = mkEnableOption "Enable avahi profile";
    };
  };
  config = mkIf cfg.enable {
    services = {
      avahi = {
        enable = true;
        ipv4 = true;
        ipv6 = true;
        nssmdns4 = true;
        publish = {
          enable = true;
          userServices = true;
        };
      };
    };
  };
}
