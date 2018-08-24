{ pkgs, ... }:

{
  imports = [
    ./base.nix
    ./dev.nix
    ./fish.nix
    ./ssh.nix
  ];
}
