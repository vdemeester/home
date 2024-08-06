{ config, lib, pkgs, nixosConfig, ... }:

let
  inherit (lib) optionals;
in
{
  imports = [
    # autorandr
    ./finances.nix
    ./firefox.nix
    ./gtk.nix
    ./keyboard.nix
    ./mpv.nix
    ./spotify.nix
    ./passwordstore.nix
    ./audio.nix
  ]
  ++ optionals nixosConfig.modules.desktop.xorg.enable [ ./xorg.nix ]
  ++ optionals nixosConfig.modules.desktop.wayland.enable [ ./wayland.nix ]
  ++ optionals nixosConfig.modules.desktop.wayland.sway.enable [ ./sway.nix ]
  ++ optionals nixosConfig.modules.desktop.wayland.hyprland.enable [ ./hyprland.nix ];

  home.sessionVariables = {
    WEBKIT_DISABLE_COMPOSITING_MODE = 1;
  };
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    desktop-file-utils
    go-jira
    hunspell
    hunspellDicts.en_GB-ize
    hunspellDicts.en_US-large
    hunspellDicts.fr-any
    keybase
    libreoffice-fresh
    mpw
    my.batzconverter
    # nyxt
    obs-studio
    playerctl
    profile-sync-daemon
    signal-desktop
    thunderbird
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

  home.file.".XCompose".source = ./xorg/XCompose;
  xdg.configFile."xorg/emoji.compose".source = ./xorg/emoji.compose;
  xdg.configFile."xorg/parens.compose".source = ./xorg/parens.compose;
  xdg.configFile."xorg/modletters.compose".source = ./xorg/modletters.compose;
  home.file.".local/share/applications/google-meet.desktop".source = ./xorg/google-meet.desktop;
  home.file.".local/share/applications/chrome-slack.desktop".source = ./xorg/chrome-slack.desktop;
  home.file.".local/share/applications/notion.desktop".source = ./xorg/notion.desktop;
  home.file.".local/share/applications/todoist.desktop".source = ./xorg/todoist.desktop;
  home.file.".oath" = {
    source = config.lib.file.mkOutOfStoreSymlink "/home/vincent/desktop/documents/.oath";
    recursive = true;
  };
}
