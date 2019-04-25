{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.cloud.google.enable = true;
  profiles.containers = {
    enable = true;
    docker = true;
    kubernetes = { enable = true; minikube.enable = true; };
    openshift = { enable = true; minishift.enable = true; };
  };
  profiles.dev = {
    go.enable = true;
    java = { enable = true; idea = true; };
    js.enable = true;
    haskell.enable = true;
    python.enable = true;
    rust.enable = true;
    vscode.enable = true;
  };
  profiles.finances.enable = true;
  profiles.laptop.enable = true;
  profiles.media.enable = true;
  profiles.mails.enable = true;
  programs = {
    google-chrome.enable = true;
    podman.enable = true;
  };
  home.packages = with pkgs; [
    obs-studio
    slack
    awscli
    terraform
    nur.repos.vdemeester.openshift-installer
    nur.repos.vdemeester.operator-sdk
    gnome3.vinagre
  ];
  services.shairport-sync.enable = true;
}
