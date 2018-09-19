{ pkgs, ... }:

{
  imports = [
    ./laptop.nix
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
  ];
  home.packages = with pkgs; [
    vscode
  ];
}
