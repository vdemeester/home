{ globals, ... }:
{
  # FIXME move the "networks" to globals
  services.bind = {
    enable = true;
    forwarders = [
      "8.8.8.8"
      "8.8.4.4"
    ];
    extraOptions = ''
      dnssec-validation no;
    '';
    cacheNetworks = [
      "127.0.0.0/8"
    ]
    ++ globals.net.dns.cacheNetworks;
    inherit (globals.net.dns) zones;
  };
}
