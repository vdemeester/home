{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tektoncd-cli
    kubernetes-helm
    # my.tkn
    my.tkn-pac
    my.tkn-local
    rekor-cli
    cosign
  ];
}
