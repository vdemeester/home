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
  profiles.containers = {
    enable = true;
    docker = true;
    kubernetes = { enable = true; minikube.enable = true; };
    #openshift = { enable = true; minishift.enable = true; };
  };
  programs.vscode.enable = true;
  home.packages = with pkgs; [
    google-chrome
    obs-studio
    mattermost-desktop
    slack
    virtmanager
  ];
  services.shairport-sync.enable = true;
}
