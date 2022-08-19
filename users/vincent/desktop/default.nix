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
  ]
  ++ optionals nixosConfig.modules.desktop.xorg.enable [ ./xorg.nix ]
  ++ optionals nixosConfig.profiles.desktop.i3.enable [ ./i3.nix ./xorg.nix ]
  ++ optionals nixosConfig.modules.desktop.wayland.sway.enable [ ./sway.nix ]
  ++ optionals nixosConfig.profiles.desktop.gnome.enable [ (import ./gnome.nix) ];

  home.sessionVariables = { WEBKIT_DISABLE_COMPOSITING_MODE = 1; };
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    desktop-file-utils
    hunspell
    hunspellDicts.en_US-large
    hunspellDicts.en_GB-ize
    hunspellDicts.fr-any
    libreoffice-fresh
    xdg-user-dirs
    xdg_utils
    xsel
    obs-studio
    signal-desktop
    keybase
    profile-sync-daemon
    youtube-dl
    my.batzconverter
    mpw
    go-jira
    thunderbird
  ];

  home.file.".XCompose".source = ./xorg/XCompose;
  xdg.configFile."xorg/emoji.compose".source = ./xorg/emoji.compose;
  xdg.configFile."xorg/parens.compose".source = ./xorg/parens.compose;
  xdg.configFile."xorg/modletters.compose".source = ./xorg/modletters.compose;
  home.file.".local/share/applications/google-meet.desktop".source = ./xorg/google-meet.desktop;
  home.file.".oath" = {
    source = config.lib.file.mkOutOfStoreSymlink "/home/vincent/desktop/documents/.oath";
    recursive = true;
  };
}
