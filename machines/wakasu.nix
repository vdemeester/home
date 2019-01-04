{ pkgs, ...}:

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
    kubernetes = { enable = true; minikube.enable = true; };
    openshift = { enable = true; minishift.enable = true; };
  };
  profiles.media.enable = true;
  programs = {
    google-chrome.enable = true;
    podman.enable = true;
  };
  home.packages = with pkgs; [
    my-obs
    mattermost-desktop
    slack
    virtmanager
    awscli
    terraform
  ];
  services.shairport-sync.enable = true;
}
