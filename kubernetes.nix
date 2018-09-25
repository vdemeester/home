{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    go-containerregistry
    kail
    minikube
  ];
}
