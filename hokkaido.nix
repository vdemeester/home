{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    # k8s
    ./kubernetes.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev = {
    go.enable = true;
    js.enable = true;
    java.enable = true;
    python.enable = true;
    rust.enable = true;
  };
  profiles.containers.enable = true;
  programs.vscode.enable = true;
  home.packages = with pkgs; [
    google-chrome
  ];
}
