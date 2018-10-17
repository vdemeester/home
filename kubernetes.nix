{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    cri-tools
    kail
    kubectl
    kustomize
    knctl
    kube-prompt
  ];
}
