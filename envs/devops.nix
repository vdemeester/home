{ pkgs, prefix, ... }:

{
  home.packages = with pkgs; [
    packer
    google-cloud-sdk
  ];
}
