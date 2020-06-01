{ pkgs, ... }:

{
  imports = [
    ./gcloud.nix
    ./kubernetes.nix
    ./openshift.nix
  ];

  home.packages = with pkgs; [
    podman
    skopeo
  ];
}
