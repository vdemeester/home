{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tektoncd-cli
    kubernetes-helm
    # my.tkn
    my.tkn-pac
    my.tkn-local
    # my.sugarjazy
    rekor-cli
    cosign
  ];
}
