{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "aion";

    network = {
      localIPs = [ "192.168.1.49" ];
      dnsNames = [
        "aion.home"
        "aion.vpn"
        "aion.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "T8qfsBiOcZNxUeRHFg+2FPdGj4AuGloJ4b+0uI2jM2w=";
        ips = [ "10.100.0.49" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIMs2o62unBFN/LHRg3q2N4QyZW0+DC/gjw3yzRbWdzx5";
    };

    syncthing = {
      enable = true;
      deviceID = "YORNSGU-UC4IAG5-IWJCD7T-MVPIU7O-AYM36UK-LEHF7AP-CBC4L6C-ZWKUYQF";
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
