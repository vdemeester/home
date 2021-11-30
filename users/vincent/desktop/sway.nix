{ config, nixosConfig, lib, pkgs, ... }:

{
  wayland.windowManager.sway.enable = true;
  wayland.windowManager.sway.config = {
    gaps = {
      inner = 2;
      outer = 2;
    };
    modifier = "Mod4";
  };
  home.packages = with pkgs; [
    alacritty
    kitty
  ];
}
