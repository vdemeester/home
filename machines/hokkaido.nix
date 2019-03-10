{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev = {
    go.enable = true;
    rust.enable = true;
  };
  profiles.gpg.enable = true;
  profiles.containers.enable = true;
  profiles.media.enable = true;
  profiles.mails.enable = true;
  programs.podman.enable = true;
  home.packages = with pkgs; [
    ledger
    slack
  ];
}
