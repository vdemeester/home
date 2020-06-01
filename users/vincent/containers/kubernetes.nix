{ pkgs, ... }:

{
  home.packages = with pkgs; [
    #cri-tools
    kail
    # kubectl # FIXME this goes against oc
    kustomize
    kubectx
    my.ko
    my.krew
    my.kss
    my.kubernix
  ];
}
