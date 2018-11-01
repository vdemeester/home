{ pkgs, prefix, ...}:

{
  imports = [
    ./desktop.nix
    ./devops.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
    ./dev.haskell.nix
    ./openshift.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev.go.enable = true;
  programs.vscode.enable = true;
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    google-chrome
    obs-studio
    mattermost-desktop
    slack
    virtmanager
    jetbrains.idea-ultimate
  ];
  services.shairport-sync.enable = true;
}
