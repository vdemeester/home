{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.ssh;
in
{
  options = {
    profiles.ssh = {
      enable = mkEnableOption "Enable ssh profile";
      listenAddresses = mkOption {
        type = types.listOf types.str;
        default = [ ];
      };
      forwardX11 = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to allow X11 connections to be forwarded.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    warnings = [ "The option 'profiles.ssh' is deprecated, use 'modules.services.ssh' instead" ];
    modules.services.ssh = {
      enable = cfg.enable;
      listenAddresses = cfg.listenAddresses;
      forwardX11 = cfg.forwardX11;
    };
  };
}
