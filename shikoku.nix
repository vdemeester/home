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
    ./openshift.nix
    ./ssh.nix
    ./vscode.nix
  ];
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    google-chrome
    obs-studio # screencast
    jetbrains.idea-ultimate
    mattermost-desktop
    slack
    zoom-us
  ];
}
