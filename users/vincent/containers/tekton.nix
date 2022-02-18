{ pkgs, ... }:

{
  home.packages = with pkgs; [
    # tektoncd-cli
    my.tkn
    my.tkn-pac
  ];
}
