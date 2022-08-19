{ config, lib, pkgs, ... }:
let
  inherit (lib) mkEnableOption mkIf mkOption types;
  cfg = config.modules.hardware.laptop;
in
{
  options = {
    modules.hardware.laptop = {
      enable = mkEnableOption "Enable laptop profile";
    };
  };
  config = mkIf cfg.enable { };
}
