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
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.aion.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };

  networking.useDHCP = lib.mkDefault true;

  environment.systemPackages = with pkgs; [
    lm_sensors
    gnumake
  ];

}
