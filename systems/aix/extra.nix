{ globals, ... }:
{
  imports = [
    ../common/services/prometheus-exporters-node.nix
  ];

  networking.firewall.enable = false;

  services = {
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.aix.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.net.vpn.pubkey}";
    };
  };

  # TODO: could be enable by default for all ?
  security.pam.enableSSHAgentAuth = true;

  security.apparmor.enable = true;
}
