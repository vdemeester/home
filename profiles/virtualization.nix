# Virtualization configuration
{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    qemu
    vde2
  ];
}
