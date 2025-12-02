{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "rhea";

    network = {
      localIPs = [ "192.168.1.50" ];
      dnsNames = [
        "rhea.home"
        "rhea.vpn"
        "rhea.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "QBGdlPgtaLIh+WDLbuIWPL+Nr08mtfIqs6RwgVDAGjA=";
        ips = [ "10.100.0.50" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIKFH3Lk4bRgNyFRK/Hzg1PvVbL/dpyI1SmLJFkb6VQDw";
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
        photos = {
          path = "/neo/pictures/photos";
        };
        music = {
          path = "/neo/music";
        };
      };
    };
  };
}
