{
  config,
  pkgs,
  desktop,
  ...
}:
{
  imports = [
    # FIXME why the infinite recusion
    (./. + "/${desktop}/default.nix")

    ./firefox.nix
    ./gtk.nix
    ./kitty.nix
    ./mails.nix
    ./mpv.nix
    ./passage.nix
    ./xdg.nix

    ../dev/default.nix
    ../dev/desktop.nix
  ];

  home.packages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.fr
    dino
    ffmpeg-full
    hunspell
    hunspellDicts.en_GB-ize
    hunspellDicts.en_US-large
    hunspellDicts.fr-any
    libnotify
    libosinfo
    loupe
    p7zip
    papers
    playerctl
    pwvucontrol
    trash-cli
    yt-dlp
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
    poweralertd.enable = true;
    gnome-keyring.enable = true;
  };

  home.file.".oath" = {
    source = config.lib.file.mkOutOfStoreSymlink "/home/vincent/desktop/documents/.oath";
    recursive = true;
  };
}
