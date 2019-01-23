{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.virtualization;
in
{
  options = {
    profiles.virtualization = {
      enable = mkOption {
        default = false;
        description = "Enable virtualization profile";
        type = types.bool;
      };
      listenTCP = mkOption {
        default = false;
        description = "Make libvirt listen to TCP";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    virtualisation.libvirtd = {
      enable = true;
      extraConfig = mkIf cfg.listenTCP ''
      listen_tcp = 1
      tcp_port = "16509"
      '';
    };
    environment.systemPackages = with pkgs; [
      qemu
      vde2
    ];
  };
}
