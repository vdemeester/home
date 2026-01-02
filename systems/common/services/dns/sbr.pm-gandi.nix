# Gandi (public) DNS zone for sbr.pm - uses VPN IPs (10.100.0.x) only
{ dns, globals, ... }:
let
  dnsHelpers = import ../../../../lib/dns-helpers.nix { inherit globals; };
  inherit (dnsHelpers) getMachineIP;

  # Import the common zone and override immich to point to kerkouane's public IP
  baseZone = import ./sbr.pm-common.nix {
    inherit dns globals;
    getIPForMachine = getMachineIP;
  };
in
baseZone
// {
  subdomains = baseZone.subdomains // {
    # Override immich to point to kerkouane's public IP (reverse proxy entry point)
    immich.A = [ "167.99.17.238" ];
  };
}
