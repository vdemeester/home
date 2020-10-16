{ pkgs, ... }:

{
  home.packages = with pkgs; [
    #cri-tools
    kail
    kubectl
    kustomize
    kubectx
    kind
    minikube
    my.ko
    my.krew
    my.kss
  ];
}
