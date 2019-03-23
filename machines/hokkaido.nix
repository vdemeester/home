{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.containers.enable = true;
  profiles.dev = {
    go.enable = true;
    rust.enable = true;
    java = { enable = true; javaPackage = pkgs.jre; };
  };
  profiles.laptop.enable = true;
  profiles.media.enable = true;
  profiles.mails = {
    enable = true;
    frequency = "hourly";
  };
  programs.podman.enable = true;
  home.packages = with pkgs; [
    ledger
    slack
  ];
}
