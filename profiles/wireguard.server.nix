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
  networking.wireguard.interfaces = with import ../assets/machines.nix; {
    "wg0" = {
      ips = wg.allowedIPs;
      listenPort = wg.listenPort;
      privateKeyFile = "/etc/nixos/wireguard.private.key";
      peers = wg.peers;
    };
  };
}
