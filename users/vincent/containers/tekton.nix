{ pkgs, ... }:

{
  home.packages = with pkgs; [
    my.tkn
    my.tkn_oci
  ];
}
