{ config, pkgs, ... }:

{
  imports = [
    ./wireguard.nix
  ];
  
  boot.kernel.sysctl."net.ipv4.ip_forward" = 1;
  networking.firewall.extraCommands = ''
    iptables -t nat -A POSTROUTING -s10.100.0.0/24 -j MASQUERADE
  '';
  networking.firewall.allowedUDPPorts = [ 51820 ];
  networking.wireguard.interfaces = with import ../assets/wireguard.nix; {
    "wg0" = {
      ips = kerkouane.allowedIPs;
      listenPort = main.listenPort;
      privateKeyFile = "/etc/nixos/wireguard.private.key";
      peers = main.peers;
    };
  };
}