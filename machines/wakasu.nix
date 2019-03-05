{ pkgs, ... }:

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
    vscode.enable = true;
  };
  profiles.cloud.google.enable = true;
  profiles.containers = {
    enable = true;
    docker = true;
    kubernetes = { enable = true; minikube.enable = true; minikube.package = pkgs.nur.repos.vdemeester.minikube; };
    openshift = { enable = true; minishift.enable = true; };
  };
  profiles.media.enable = true;
  profiles.mails.enable = true;
  programs = {
    google-chrome.enable = true;
    podman.enable = true;
  };
  home.packages = with pkgs; [
    obs-studio
    slack
    virtmanager
    awscli
    terraform
  ];
  services.shairport-sync.enable = true;
}
