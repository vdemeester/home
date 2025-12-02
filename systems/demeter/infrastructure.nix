{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "demeter";

    network = {
      localIPs = [ "192.168.1.182" ];
      dnsNames = [
        "demeter.home"
        "demeter.vpn"
        "demeter.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "/bBh4gvDty/AA2qIiHc7K0OHoOXWmj2SFFXdDq8nsUU=";
        ips = [ "10.100.0.82" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGqQfEyHyjIGglayB9FtCqL7bnYfNSQlBXks2IuyCPmd";
    };

    syncthing = {
      enable = true;
      deviceID = "TXCV3TS-TUEOTH6-ETB3LBV-KCIHT4L-RCCOIE3-VPBCNJB-VHQEAYI-WOXK5A5";
      folders = {
        sync = {
          type = "receiveonly";
        };
      };
    };
  };
}
