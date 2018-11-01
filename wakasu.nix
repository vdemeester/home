{ pkgs, prefix, ...}:

{
  imports = [
    ./desktop.nix
    ./devops.nix
    ./openshift.nix
  ];
  profiles.laptop.enable = true;
  profiles.dev = {
    go.enable = true;
    java = { enable = true; idea = true; };
    js.enable = true;
    haskell.enable = true;
    python.enable = true;
    rust.enable = true;
  };
  profiles.docker.enable = true;
  programs.vscode.enable = true;
  programs.podman.enable = true;
  home.packages = with pkgs; [
    google-chrome
    obs-studio
    mattermost-desktop
    slack
    virtmanager
  ];
  services.shairport-sync.enable = true;
}
