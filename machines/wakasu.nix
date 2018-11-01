{ pkgs, prefix, ...}:

{
  imports = [
    ./base.nix
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
  profiles.cloud.google.enable = true;
  profiles.containers = {
    enable = true;
    docker = true;
    kubernetes = { enable = true; minikube.enable = true; };
    openshift = { enable = true; minishift.enable = true; };
  };
  profiles.media.enable = true;
  programs = {
    vscode.enable = true;
    google-chrome.enable = true;
  };
  home.packages = with pkgs; [
    obs-studio
    mattermost-desktop
    slack
    virtmanager
  ];
  services.shairport-sync.enable = true;
}
