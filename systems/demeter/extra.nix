{ globals, ... }:
{
  imports = [
    ../common/services/bind.nix
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-bind.nix
  ];

  networking.firewall.enable = false;

  services = {
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.athena.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };

  # TODO: could be enable by default for all ?
  security.pam.enableSSHAgentAuth = true;

  security.apparmor.enable = true;
}
