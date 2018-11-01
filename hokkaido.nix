{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    # dev
    ./dev.python.nix
    ./dev.js.nix
    # k8s
    ./containers.nix
    ./kubernetes.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev.go.enable = true;
  programs.vscode.enable = true;
  home.packages = with pkgs; [
    google-chrome
  ];
}
