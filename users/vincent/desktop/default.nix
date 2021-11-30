{ lib, pkgs, nixosConfig, ... }:

{
  imports = [
    # autorandr
    ./finances.nix
    ./firefox.nix
    ./gtk.nix
    ./keyboard.nix
    ./mpv.nix
    ./spotify.nix
  ] ++ lib.optionals nixosConfig.profiles.desktop.i3.enable [ ./i3.nix ]
  ++ lib.optionals nixosConfig.profiles.desktop.sway.enable [ ./sway.nix ];

  home.sessionVariables = { WEBKIT_DISABLE_COMPOSITING_MODE = 1; };
  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
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
  ];

  programs.autorandr.enable = nixosConfig.profiles.laptop.enable;

  home.file.".XCompose".source = ./xorg/XCompose;
  # home.file.".Xmodmap".source = ./xorg/Xmodmap;
  xdg.configFile."xorg/emoji.compose".source = ./xorg/emoji.compose;
  xdg.configFile."xorg/parens.compose".source = ./xorg/parens.compose;
  xdg.configFile."xorg/modletters.compose".source = ./xorg/modletters.compose;
  home.file.".local/share/applications/google-meet.desktop".source = ./xorg/google-meet.desktop;
}
