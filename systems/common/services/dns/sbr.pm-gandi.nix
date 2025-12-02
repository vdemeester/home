# Gandi (public) DNS zone for sbr.pm - uses VPN IPs (10.100.0.x) only
{ dns, config, ... }:
let
  dnsHelpers = import ../../../../lib/dns-helpers.nix { inherit config; };
  inherit (dnsHelpers) getMachineIP;
in
import ./sbr.pm-common.nix {
  inherit dns config;
  getIPForMachine = getMachineIP;
}
