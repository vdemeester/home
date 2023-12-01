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
    kind
    # minikube # probably don't need that always.. only on demand
    ko
    crane
    krew
    kss
    # our own scripts
    knd
    bekind
    stern
    my.chmouzies.kubernetes
    kubectx
  ];
  programs.zsh.initExtra = ''
    alias -g SK="|snazy -s --kail --kail-prefix-format='{pod}'"
  '';
}
