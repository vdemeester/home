{ pkgs, ... }:

{
  home.packages = with pkgs; [
    tektoncd-cli
    #my.tkn_oci
  ];
}
