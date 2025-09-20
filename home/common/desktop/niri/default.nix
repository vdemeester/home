{ config, pkgs, ... }:
{
  imports = [
    ../sway/mako.nix
    ../sway/swayidle.nix
    ../sway/rofi.nix
  ];

  home.packages = with pkgs; [
    pinentry-gnome3
    wf-recorder
    wl-clipboard
    wl-kbptr
    wtype
    wlprop

    zenity
    wofi
    swaybg
  ];

  services = {
    avizo.enable = true;
    wlsunset = {
      enable = true;
      latitude = "48.87";
      longitude = "2.33";
    };
    cliphist = {
      enable = true;
      package = pkgs.cliphist;
    };
  };
  programs.niri = {
    enable = true;
    package = pkgs.niri-stable;
    config = null; # FIXME I will need to migrate this
  };
}
