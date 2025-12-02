{ ... }:
{
  infrastructure.machine = {
    enable = true;
    hostname = "athena";

    network = {
      localIPs = [ "192.168.1.183" ];
      dnsNames = [
        "athena.home"
        "athena.vpn"
        "athena.sbr.pm"
      ];

      vpn = {
        enable = true;
        publicKey = "RWqH7RdIXg+YE9U1nlsNiOC7jH8eWjWQmikqBVDGSXU=";
        ips = [ "10.100.0.83" ];
      };
    };

    ssh = {
      hostKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIM/4KRP1rzOwyA2zP1Nf1WlLRHqAGutLtOHYWfH732xh";
    };

    syncthing = {
      enable = true;
      deviceID = "N3AMUVI-FM2BAOD-U3OMZDJ-UHMQE6J-ACMM5B7-S7BTK6P-PSM36NR-DVZHLQF";
      folders = {
        sync = {
          type = "receiveonly";
        };
      };
    };
  };
}
