# Virtualization configuration
{ config, pkgs, ... }:

{
  virtualisation.libvirtd = {
    enable = true;
  };
  environment.systemPackages = with pkgs; [
    qemu
    vde2
  ];
}
