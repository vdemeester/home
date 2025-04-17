{ config, pkgs, desktop, ... }:
{
  imports = [
    # FIXME why the infinite recusion
    # (./. + "/${desktop}")
    ./sway

    ./firefox.nix
    ./gtk.nix
    ./kitty.nix
    ./mpv.nix
    ./passage.nix
    ./xdg.nix
  ];

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    ffmpeg-full
    hunspell
    hunspellDicts.en_GB-ize
    hunspellDicts.en_US-large
    hunspellDicts.fr-any
    libnotify
    loupe
    papers
    playerctl
    pwvucontrol
    trash-cli
    yt-dlp

    unstable.signal-desktop
  ];

  home.pointerCursor = {
    gtk.enable = true;
    x11.enable = true;
    package = pkgs.qogir-icon-theme;
    name = "Qogir";
    size = 24;
  };

  services = {
    udiskie.enable = true;
  };

  home.file.".oath" = {
    source = config.lib.file.mkOutOfStoreSymlink "/home/vincent/desktop/documents/.oath";
    recursive = true;
  };
}
