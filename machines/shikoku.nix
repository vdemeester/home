{ pkgs, ... }:

{
  imports = [
    ./base.nix
  ];
  profiles.cloud.google.enable = true;
  profiles.containers = {
    enable = true;
    docker = true;
    podman = true;
    kubernetes = { enable = true; minikube.enable = true; };
    openshift = { enable = true; minishift.enable = true; };
  };
  profiles.desktop.enable = true;
  profiles.dev = {
    go.enable = true;
    haskell.enable = true;
    python.enable = true;
    rust.enable = true;
    vscode.enable = true;
  };
  profiles.gaming.enable = true;
  profiles.media.enable = true;
  services.shairport-sync.enable = true;
}
