{ pkgs, ... }:

let
  my-obs = pkgs.obs-studio.overrideDerivation (oldAttrs: {
      patches = [
        (pkgs.fetchpatch {
          url = "https://patch-diff.githubusercontent.com/raw/obsproject/obs-studio/pull/1557.diff";
          sha256 = "162fnkxh2wyn6wrrm1kzv7c2mn96kx35vlmk2qwn1nqlifbpsfyq";
        })
      ];
    });
in
{
  imports = [
    ./base.nix
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
    vscode.enable = true;
  };
  profiles.media.enable = true;
  profiles.cloud.google.enable = true;
  profiles.containers = {
    enable = true;
    docker = true;
    podman = true;
    kubernetes = { enable = true; minikube.enable = true; };
    openshift = { enable = true; minishift.enable = true; };
  };
  programs = {
    google-chrome.enable = true;
  };
  services.shairport-sync.enable = true;
  
  home.file."src/github.com/knative/.envrc".source = ../projects/nix.envrc;
  home.file."src/github.com/knative/default.nix".source = ../projects/knative/default.nix;

  home.packages = with pkgs; [
    my-obs
    virtmanager
  ];
}
