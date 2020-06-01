{ pkgs, ... }:

{
  imports = [
    ./gcloud.nix
    ./kubernetes.nix
    ./openshift.nix
  ];
}
