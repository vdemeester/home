{
  libx,
  globals,
  ...
}:
{
  imports = [
    ../common/services/bind.nix
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-bind.nix
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  services = {
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.athena.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };
}
