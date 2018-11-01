{ pkgs, prefix, ... }:

{
  imports = [ ./kubernetes.nix ./minikube.nix ];
  home.packages = with pkgs; [
    google-cloud-sdk
    packer
    spice_gtk
  ];
}
