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
    services = {
      avahi = {
        enable = true;
        ipv4 = true;
        ipv6 = true;
        nssmdns = true;
        publish = {
          enable = true;
          userServices = true;
        };
      };
    };
  };
}
