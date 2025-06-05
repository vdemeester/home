{ globals, ... }:
{
  imports = [
    ../common/services/samba.nix
    ../common/services/prometheus-exporters-node.nix
  ];

  networking.firewall.enable = false;

  services = {
    samba.settings."vincent" = {
      path = "/data/share";
      public = true;
      browseable = "yes";
      "read only" = "no";
      "guest ok" = "yes";
      writable = true;
      comment = "Vincent's share";
      "create mask" = "0644";
      "directory mask" = "0755";
      "force user" = "vincent";
      "force group" = "users";
    };
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
