# Local DNS zone for sbr.pm - uses local IPs (192.168.1.x) with VPN fallback
{ dns, globals, ... }:
let
  # Helper to get local IP, fallback to VPN IP
  getLocalMachineIP =
    machine:
    let
      localIps = machine.net.ips or [ ];
      vpnIps = machine.net.vpn.ips or [ ];
      ips = if localIps != [ ] then localIps else vpnIps;
    in
    if builtins.isList ips then builtins.head ips else ips;
in
import ./sbr.pm-common.nix {
  inherit dns globals;
  getIPForMachine = getLocalMachineIP;
}
