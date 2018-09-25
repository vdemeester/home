{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    cri-tools
    go-containerregistry
    kail
    minikube
  ];
}
