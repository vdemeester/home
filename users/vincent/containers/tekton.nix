{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tektoncd-cli
    kubernetes-helm
    snazy
    # my.tkn
    my.tkn-pac
    my.tkn-local
    rekor-cli
    cosign
  ];
}
