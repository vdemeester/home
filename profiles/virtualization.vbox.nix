{ config, pkgs, ... }:

{
  virtualisation = {
    virtualbox.host.enable = true;
    nixpkgs.config.virtualbox.enableExtensionPack = true;
    networking.firewall.trustedInterfaces = [ "vboxnet0" ];
    environment = {
      systemPackages = with pkgs; [
        vagrant
      ];
    };
  }
}
