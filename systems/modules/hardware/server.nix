{ config, lib, ... }:
let
  inherit (lib) mkEnableOption mkIf;
  cfg = config.modules.hardware.server;
in
{
  options = {
    modules.hardware.server = {
      enable = mkEnableOption "Enable server profile";
    };
  };
  config = mkIf cfg.enable { };
}
