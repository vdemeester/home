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
    java.enable = true;
    js.enable = true;
    python.enable = true;
    rust.enable = true;
  };
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
