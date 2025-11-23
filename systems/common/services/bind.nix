{
  globals,
  inputs,
  ...
}:
let
  dns = inputs.dns;

  # Generate zone file content using dns.nix
  mkZone = zoneName: zoneFile: dns.lib.toString zoneName (import zoneFile { inherit dns globals; });
in
{
  services.bind = {
    enable = true;
    forwarders = [
      "8.8.8.8"
      "8.8.4.4"
    ];
    extraOptions = ''
      dnssec-validation no;
    '';
    cacheNetworks = [ "127.0.0.0/8" ] ++ globals.net.dns.cacheNetworks;

    zones = [
      # sbr.pm zone
      {
        name = "sbr.pm";
        master = true;
        file = mkZone "sbr.pm" ./dns/sbr.pm.nix;
      }
      # home zone
      {
        name = "home";
        master = true;
        file = mkZone "home" ./dns/home.nix;
      }
      # home reverse zone
      {
        name = "192.168.1.in-addr.arpa";
        master = true;
        file = mkZone "192.168.1.in-addr.arpa" ./dns/192.168.1.nix;
      }
      # vpn zone
      {
        name = "vpn";
        master = true;
        file = mkZone "vpn" ./dns/vpn.nix;
      }
      # vpn reverse zone
      {
        name = "10.100.0.in-addr.arpa";
        master = true;
        file = mkZone "10.100.0.in-addr.arpa" ./dns/10.100.0.nix;
      }
    ];
  };
}
