# Gandi (public) DNS zone for sbr.pm - uses VPN IPs (10.100.0.x) only
{ dns, globals, ... }:
let
  dnsHelpers = import ../../../../lib/dns-helpers.nix { inherit globals; };
  inherit (dnsHelpers) getMachineIP;
in
import ./sbr.pm-common.nix {
  inherit dns globals;
  getIPForMachine = getMachineIP;
}
