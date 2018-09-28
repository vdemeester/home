{ pkgs, prefix, ... }:

{
  imports = [ ./containers.nix ./kubernetes.nix ./minikube.nix ];
  home.packages = with pkgs; [
    google-cloud-sdk
    packer
    spice_gtk
  ];
}
