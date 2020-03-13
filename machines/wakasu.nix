{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.zsh = {
    enable = true;
  };
  profiles.audio = {
    enable = true;
    mpd = {
      enable = true;
      musicDir = "/mnt/synodine/volumeUSB2/usbshare/music";
    };
    shairport-sync = true;
  };
  profiles.cloud.google.enable = true;
  profiles.dev = {
    go.enable = true;
  };
  profiles.finances.enable = true;
  profiles.laptop.enable = true;
  profiles.media.enable = true;
  profiles.gpg.pinentry = "${pkgs.pinentry-gtk2}/bin/pinentry-gtk-2";
  profiles.mails = {
    enable = true;
    sync = true;
  };
  profiles.containers.kubernetes = {
    enable = true;
    krew = true;
    kind = true;
    nr = true;
  };
  programs = {
    google-chrome.enable = true;
    podman.enable = true;
  };
  home.packages = with pkgs; [
    openvpn
    krb5
    libosinfo
    virtmanager
    nur.repos.vdemeester.tkn
    thunderbird
    asciinema
    gnome3.zenity # use rofi instead
    oathToolkit
  ];
}
