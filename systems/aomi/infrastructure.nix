{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "aomi";

    network = {
      localIPs = [ "192.168.1.23" ];
      dnsNames = [
        "aomi.home"
        "aomi.vpn"
        "aomi.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "XT4D9YLeVHwMb9R4mhBLSWHYF8iBO/UOT86MQL1jnA4=";
        ips = [ "10.100.0.17" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFQVlSrUKU0xlM9E+sJ8qgdgqCW6ePctEBD2Yf+OnyME";
    };

    syncthing = {
      enable = true;
      deviceID = "CN5P3MV-EJ65J4I-OHB7OBI-LD7JBWT-7SZCZD3-Z6NAASI-UCMKOAU-X2TNNAP";
      folders = {
        org = { };
        documents = { };
        sync = { };
        screenshots = { };
        wallpapers = { };
      };
    };
  };
}
