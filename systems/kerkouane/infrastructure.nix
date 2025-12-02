{ ... }:
{
  # Enable machine infrastructure configuration
  infrastructure.machine = {
    enable = true;
    hostname = "kerkouane";

    network = {
      dnsNames = [
        "kerkouane.vpn"
        "kerkouane.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "+H3fxErP9HoFUrPgU19ra9+GDLQw+VwvLWx3lMct7QI=";
        ips = [ "10.100.0.1" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJguVoQYObRLyNxELFc3ai2yDJ25+naiM3tKrBGuxwwA";
    };

    syncthing = {
      enable = true;
      deviceID = "QGD6ICB-EPSGCEN-IQWKN77-BCRWE67-56HX5IA-E4IDBCI-WE46DK3-EC63DQ7";
      folders = {
        sync = { };
      };
    };
  };
}
