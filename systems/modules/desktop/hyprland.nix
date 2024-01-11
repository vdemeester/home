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
  config = mkIf cfg.enable {
    programs.hyprland.enable = true;

    xdg = {
      portal = {
        enable = true;
        wlr.enable = true;
        extraPortals = with pkgs; [
          # xdg-desktop-portal-wlr
          # xdg-desktop-portal-gtk
          xdg-desktop-portal-hyprland
        ];
        gtkUsePortal = true;
      };
    };
  };
}
