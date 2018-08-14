{ pkgs, prefix, ... }:

{
  imports = [
    ./server.nix
    ./dev.go.nix
  ];
}
