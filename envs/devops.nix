{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    packer
  ];
}
