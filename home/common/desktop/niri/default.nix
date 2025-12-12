{ pkgs, ... }:
{
  imports = [
    ../sway/mako.nix
    ../sway/swayidle.nix
    ../sway/rofi.nix
    ./waybar.nix
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
    rofimoji
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
      extraOptions = [
        "-max-dedupe-search"
        "100"
        "-max-items"
        "1000"
      ];
    };
  };
  programs.niri = {
    enable = true;
    package = pkgs.niri-stable;
    config = null; # FIXME I will need to migrate this
  };
}
