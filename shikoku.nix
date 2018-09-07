{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./gaming.nix
    ./devops.nix
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
    ./dev.haskell.nix
    ./fish.nix
    ./ssh.nix
  ];
  home.packages = with pkgs; [
    vscode
    zoom-us
  ];
}
