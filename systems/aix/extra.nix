{
  config,
  libx,
  ...
}:
{
  imports = [
    ../common/services/samba.nix
    ../common/services/prometheus-exporters-node.nix
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  services = {
    samba.settings = {
      global."server string" = "Aix";
      vincent = {
        path = "/data/share";
        public = "yes";
        browseable = "yes";
        "read only" = "no";
        "guest ok" = "yes";
        writable = "yes";
        comment = "Vincent's share";
        "create mask" = "0644";
        "directory mask" = "0755";
        "force user" = "vincent";
        "force group" = "users";
      };
    };
    wireguard = {
      enable = true;
      ips = libx.wg-ips config.infrastructure.machine.network.vpn.ips;
      endpoint = config.infrastructure.vpn.endpoint;
      endpointPublicKey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI="; # kerkouane
    };
  };
}
