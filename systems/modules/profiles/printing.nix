{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.printing;
in
{
  options = {
    profiles.printing = {
      enable = mkEnableOption "Enable printing profile";
    };
  };
  config = mkIf cfg.enable {
    services = {
      printing = {
        enable = true;
        drivers = [ pkgs.gutenprint ];
      };
    };
  };
}
