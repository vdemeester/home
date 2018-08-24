{ pkgs, ... }:

{
  imports = [
    ./base.nix
    ./dev.go.nix
    ./fish.nix
    ./ssh.nix
  ];
}
