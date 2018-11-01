{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./gaming.nix
    ./devops.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
    ./dev.haskell.nix
    ./openshift.nix
  ];
  profiles.desktop.enable = true;
  profiles.dev.go.enable = true;
  programs.vscode.enable = true;
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    google-chrome
    obs-studio # screencast
    jetbrains.idea-ultimate
    mattermost-desktop
    slack
    virtmanager
    zoom-us
  ];
  services.shairport-sync.enable = true;
}
