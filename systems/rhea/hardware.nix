_: {
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/44444444-4444-4444-8888-888888888888";
    fsType = "ext4";
  };

  swapDevices = [ ];

  hardware = {
    deviceTree = {
      enable = true;
      name = "rockchip/rk3588-friendlyelec-cm3588-nas.dtb";
    };
  };
}
