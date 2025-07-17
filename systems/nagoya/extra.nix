{ globals, pkgs, ... }:
{
  services = {
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.nagoya.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
  ];
}
