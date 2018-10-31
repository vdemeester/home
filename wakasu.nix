{ pkgs, prefix, ...}:

{
  imports = [
    ./laptop.nix
    ./devops.nix
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
    ./dev.haskell.nix
    ./openshift.nix
    ./ssh.nix
    ./vscode.nix
  ];
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
