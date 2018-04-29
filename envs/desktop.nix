{ pkgs, prefix, ... }:

{
  imports = [ ./fish.nix ./base.nix ];
  programs.firefox = {
    enable = true;
  };
  programs.termite = {
    enable = true;
    font = "Ubuntu Mono 16";
    sizeHints = true;
  };
  programs.rofi = {
    enable = true;
  };
  home.packages = with pkgs; [
    xdg-user-dirs
    xdg_utils
  ];
}
