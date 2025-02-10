{ config, lib, pkgs, nixosConfig, ... }:

let
  inherit (lib) optionals;
in
{
  imports = [
    ./audio.nix
    ./finances.nix
    ./firefox.nix
    ./gtk.nix
    ./keyboard.nix
    ./mpv.nix
    ./passwordstore.nix
    ./spotify.nix
  ]
  ++ optionals nixosConfig.modules.desktop.xorg.enable [ ./xorg.nix ]
  ++ optionals nixosConfig.modules.desktop.wayland.enable [ ./wayland.nix ]
  ++ optionals nixosConfig.modules.desktop.wayland.sway.enable [ ./sway.nix ];

  home.sessionVariables = {
    WEBKIT_DISABLE_COMPOSITING_MODE = 1;
  };
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    desktop-file-utils
    hunspell
    hunspellDicts.en_GB-ize
    hunspellDicts.en_US-large
    hunspellDicts.fr-any
    keybase
    mpw
    my.batzconverter
    # nyxt
    obs-studio
    playerctl
    trash-cli
    xdg-user-dirs
    xdg-utils
    xsel
    yt-dlp # youtube-dl
  ];

  xdg.portal = {
    enable = true;
    xdgOpenUsePortal = true;
    extraPortals = with pkgs; [
      xdg-desktop-portal-hyprland
      xdg-desktop-portal-gtk
    ];
    config = {
      common = {
        default = [
          "gtk"
        ];
      };
    };
  };
  xdg.mimeApps = {
    enable = true;
    defaultApplications = {
      "text/html" = "firefox.desktop";
      "x-scheme-handler/http" = "firefox.desktop";
      "x-scheme-handler/https" = "firefox.desktop";
      "x-scheme-handler/about" = "firefox.desktop";
      "x-scheme-handler/unknown" = "firefox.desktop";
    };
  };
  xdg.desktopEntries.firefox = {
    name = "Firefox";
    genericName = "Web Browser";
    exec = "firefox %U";
    terminal = false;
    categories = [ "Application" "Network" "WebBrowser" ];
    mimeType = [ "text/html" "text/xml" ];
  };
  home.file.".XCompose".source = ./xorg/XCompose;
  xdg.configFile."xorg/emoji.compose".source = ./xorg/emoji.compose;
  xdg.configFile."xorg/parens.compose".source = ./xorg/parens.compose;
  xdg.configFile."xorg/modletters.compose".source = ./xorg/modletters.compose;
  home.file.".oath" = {
    source = config.lib.file.mkOutOfStoreSymlink "/home/vincent/desktop/documents/.oath";
    recursive = true;
  };
}
