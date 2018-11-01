{ pkgs, ... }:

{
  imports = [
    ./laptop.nix
    # dev
    ./dev.go.nix
    ./dev.python.nix
    ./dev.js.nix
    ./vscode.nix
    # k8s
    ./containers.nix
    ./kubernetes.nix
  ];
  profiles.desktop.enable = true;
  home.packages = with pkgs; [
    google-chrome
  ];
}
