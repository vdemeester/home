{ config, ... }:
{
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
    fsType = "ext4";
  };
  fileSystems."/neo" = {
    device = "/dev/disk/by-uuid/7ff6e217-ec49-40af-96a1-ad28a24bb687";
    fsType = "ext4";
  };

  networking.hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName);

  swapDevices = [ ];

  hardware = {
    deviceTree = {
      enable = true;
      name = "rockchip/rk3588-friendlyelec-cm3588-nas.dtb";
    };
  };
}
