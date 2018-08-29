{ pkgs, prefix, ... }:

{
  imports = [
    ./server.nix
    ./dev.go.nix
  ];
  home.packages = with pkgs; [
    youtube-dl
  ];
}
