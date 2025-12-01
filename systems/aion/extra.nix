{
  libx,
  globals,
  lib,
  pkgs,
  ...
}:
{
  users.users.vincent.linger = true;

  services = {
    # Dual-hub client: connect to both local hub (demeter) and remote hub (kerkouane)
    wireguard.dualClient = {
      enable = true;
      ips = libx.wg-ips globals.machines.aion.net.vpn.ips;

      # Local hub (demeter)
      localHub = {
        enable = true;
        endpoint = builtins.head globals.machines.${globals.net.localHub.host}.net.ips;
        endpointPort = globals.net.localHub.port;
        endpointPublicKey = globals.machines.${globals.net.localHub.host}.net.vpn.pubkey;
      };

      # Remote hub (kerkouane)
      remoteHub = {
        endpoint = globals.net.vpn.endpoint;
        endpointPort = globals.net.vpn.port;
        endpointPublicKey = globals.machines.kerkouane.net.vpn.pubkey;
      };
    };
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
