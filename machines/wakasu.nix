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
  profiles.mails.enable = true;
  profiles.containers.kubernetes.enable = true;
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
  ];
  services.shairport-sync.enable = true;
}
