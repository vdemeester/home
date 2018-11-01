{ pkgs, ... }:

{
  imports = [
    ./desktop.nix
    ./devops.nix
    ./openshift.nix
  ];
  profiles.desktop.enable = true;
  profiles.gaming.enable = true;
  profiles.dev = {
    go.enable = true;
    haskell.enable = true;
    java = { enable = true; idea = true; };
    js.enable = true;
    python.enable = true;
    rust.enable = true;
  };
  profiles.docker.enable = true;
  programs.vscode.enable = true;
  programs.podman.enable = true;
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    google-chrome
    obs-studio # screencast
    mattermost-desktop
    slack
    virtmanager
    zoom-us
  ];
  services.shairport-sync.enable = true;
}
