{ pkgs, ... }:

{
  imports = [
    ./base.nix
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
  profiles.media.enable = true;
  programs.vscode.enable = true;
  home.packages = with pkgs; [
    google-chrome
  ];
}
