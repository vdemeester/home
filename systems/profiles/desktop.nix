{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.profiles.desktop;
in
{
  options = {
    profiles.desktop = {
      enable = mkEnableOption "desktop configuration";
    };
  };
  config = mkIf cfg.enable {
    boot = {
      # /tmp to be tmpfs
      tmpOnTmpfs = true;
      # Enable Plymouth on desktops
      plymouth.enable = true;
    };
  };
}
