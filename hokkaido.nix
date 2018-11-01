{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    # k8s
    ./containers.nix
    ./kubernetes.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev = {
    go.enable = true;
    js.enable = true;
    python.enable = true;
    rust.enable = true;
  };
  programs.vscode.enable = true;
  home.packages = with pkgs; [
    google-chrome
  ];
}
