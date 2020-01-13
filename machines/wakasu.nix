{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.zsh = {
    enable = true;
  };
  profiles.cloud.google.enable = true;
  profiles.dev = {
    go.enable = true;
  };
  profiles.finances.enable = true;
  profiles.laptop.enable = true;
  profiles.media.enable = true;
  profiles.gpg.pinentry = "${pkgs.pinentry-gtk2}/bin/pinentry-gtk-2";
  profiles.mails.enable = true;
  profiles.containers.kubernetes = {
    enable = true;
    krew.enable = true;
    kind.enable = true;
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
  services.shairport-sync.enable = true;
}
