{ config, lib, pkgs, ... }:

let
  inherit (lib) mkIf mkEnableOption mkDefault;
  cfg = config.modules.desktop.wayland.hyprland;
in
{
  options = {
    modules.desktop.wayland.hyprland = {
      enable = mkEnableOption "Enable hyprland desktop profile";
    };
  };
  config = mkIf cfg.enable
    {
      programs.hyprland.enable = true;
    };
}
