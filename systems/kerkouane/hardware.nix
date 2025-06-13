{ lib, ... }:
{
  fileSystems."/" = {
    device = "/dev/vda1";
    fsType = "ext4";
  };
  swapDevices = [
    {
      device = "/swapfile";
      size = 1024;
    }
  ];

  # START OF DigitalOcean specifics
  # This file was populated at runtime with the networking
  # details gathered from the active system.
  networking = {
    nameservers = [
      "67.207.67.2"
      "67.207.67.3"
    ];
    defaultGateway = "188.166.64.1";
    defaultGateway6 = "";
    dhcpcd.enable = false;
    usePredictableInterfaceNames = lib.mkForce true;
    interfaces = {
      eth0 = {
        ipv4.addresses = [
          {
            address = "188.166.102.243";
            prefixLength = 18;
          }
          {
            address = "10.18.0.5";
            prefixLength = 16;
          }
        ];
        ipv6.addresses = [
          {
            address = "fe80::8035:3aff:fe72:1036";
            prefixLength = 64;
          }
        ];
      };

    };
  };
  services.udev.extraRules = ''
    ATTR{address}=="82:35:3a:72:10:36", NAME="eth0"

  '';
  # END OF DigitalOcean specifics
}
