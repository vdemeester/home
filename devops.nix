{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    google-cloud-sdk
    packer
    spice_gtk
  ];
}
