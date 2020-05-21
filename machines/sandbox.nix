{ config, pkgs, ... }:

{
  networking = {
    firewall.enable = false; # we are in safe territory :D
    networkmanager = {
      dns = "dnsmasq";
    };
  };
  profiles = {
    dev.enable = true;
    nix-config.buildCores = 4;
    ssh = {
      enable = true;
      forwardX11 = true;
    };
  };
}
