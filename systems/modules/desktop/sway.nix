{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkDefault;
  cfg = config.modules.desktop.wayland.sway;
in
{
  options = {
    modules.desktop.wayland.sway = {
      enable = mkEnableOption "Enable sway desktop profile";
    };
  };
  config = mkIf cfg.enable {
    # Enable wayland desktop modules if not already
    modules.desktop.wayland.enable = true;
  };
}

