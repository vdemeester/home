{ pkgs, prefix, ... }:

{
  imports = [
    ./server.nix
  ];
  profiles.dev.go.enable = true;
  xdg.configFile."ape.conf".source = ./ape.conf;
  home.packages = with pkgs; [
    youtube-dl
  ];
}
