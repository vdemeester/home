{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    skopeo
  ];
}
