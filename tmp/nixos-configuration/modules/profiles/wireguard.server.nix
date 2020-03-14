{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.wireguard.server;
in
{
  options = {
    profiles.wireguard.server = {
      enable = mkOption {
        default = false;
        description = "Enable wireguard.server profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable {
    boot.extraModulePackages = [ config.boot.kernelPackages.wireguard ];
    environment.systemPackages = [ pkgs.wireguard ];
    boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
    networking.firewall.extraCommands = ''
      iptables -t nat -A POSTROUTING -s10.100.0.0/24 -j MASQUERADE
    '';
    networking.firewall.allowedUDPPorts = [ 51820 ];
    networking.firewall.trustedInterfaces = [ "wg0" ];
    networking.wireguard.interfaces = with import ../../assets/machines.nix; {
      "wg0" = {
        ips = wireguard.kerkouane.allowedIPs;
        listenPort = wg.listenPort;
        privateKeyFile = "/etc/nixos/wireguard.private.key";
        peers = wg.peers;
      };
    };
  };
}
