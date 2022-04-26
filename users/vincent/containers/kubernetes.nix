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
    my.kail
    kubectl
    kustomize
    kind
    minikube
    ko
    crane
    #my.krew
    my.kss
    # our own scripts
    knd
    bekind
    stern
    my.chmouzies.kubernetes
  ];
  programs.zsh.initExtra = ''
    alias -g SK="|sugarjazy -s --kail --kail-prefix-format='{pod}'"
  '';
}
