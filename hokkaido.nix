{ pkgs, ... }:

{
  imports = [
    ./laptop.nix
    ./devops.nix
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
    ./dev.haskell.nix
  ];
  home.packages = with pkgs; [
    vscode
  ];
}
