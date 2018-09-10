{ pkgs, prefix, ... }:

{
  imports = [
    ./server.nix
    ./dev.go.nix
  ];
  xdg.configFile."ape.conf".source = ./ape.conf;
  home.packages = with pkgs; [
    youtube-dl
  ];
}
