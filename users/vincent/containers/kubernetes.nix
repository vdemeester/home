{ config, lib, pkgs, ... }:

with lib;
let
  knd = pkgs.writeScriptBin "knd" ''
    #!${pkgs.stdenv.shell}
    ${pkgs.kubectl}/bin/kubectl get namespaces -o name | ${pkgs.fzf}/bin/fzf --multi | xargs kubectl delete
  '';
in
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
    # our own scripts
    knd
    bekind
  ];
}
