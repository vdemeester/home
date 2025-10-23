{ pkgs, ... }:
{
  home.packages = with pkgs; [
    tektoncd-cli
    tektoncd-cli-pac
  ];
}
