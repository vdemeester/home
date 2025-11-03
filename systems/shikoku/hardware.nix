{
  inputs,
  lib,
  config,
  ...
}:
{
  imports = [
    inputs.disko.nixosModules.disko
    (import ./disks.nix { inherit lib; })
  ];
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/73fd8864-f6af-4fdd-b826-0dfdeacd3c19";
    fsType = "ext4";
    options = [
      "noatime"
      "discard"
    ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/829D-BFD1";
    fsType = "vfat";
  };

  # Extra data
  # HDD:   b58e59a4-92e7-4278-97ba-6fe361913f50
  fileSystems."/data" = {
    device = "/dev/disk/by-uuid/b58e59a4-92e7-4278-97ba-6fe361913f50";
    fsType = "ext4";
    options = [ "noatime" ];
  };
  # ZFS Pool
  # SSD1:  469077df-049f-4f5d-a34f-1f5449d782ec
  # SSD2:  e11a3b63-791c-418b-9f4b-5ae0199f1f97
  # NVME2: 3d2dff80-f2b1-4c48-8e76-12b01fdf4137
  # boot.zfs.extraPools = [ "tank" ];
  # networking.hostId = "03129692bea040488878aa0133e54914";
  # networking.hostId = "03129692";
  # fileSystems."/tank/data" =
  #   {
  #     device = "tank/data";
  #     fsType = "zfs";
  #     options = [ "zfsutil" ];
  #   };
  #
  # fileSystems."/tank/virt" =
  #   {
  #     device = "tank/virt";
  #     fsType = "zfs";
  #     options = [ "zfsutil" ];
  #   };

  swapDevices = [
    {
      device = "/dev/disk/by-uuid/a9ec44e6-0c1d-4f60-9f5c-81a7eaa8e8fd";
    }
  ];

  networking = {
    hostId = builtins.substring 0 8 (builtins.hashString "md5" config.networking.hostName);
    # Bridge setup
    bridges.br1.interfaces = [ "enp0s31f6" ];
    useDHCP = false;
    interfaces.br1 = {
      useDHCP = true;
    };
    # FIXME probably change this
    firewall.enable = false; # we are in safe territory :D
  };
  hardware.nvidia = {
    modesetting.enable = true;
    open = false;
    nvidiaSettings = true;
    package = config.boot.kernelPackages.nvidiaPackages.stable;
  };
  hardware.graphics = {
    enable = true;
  };
  nixpkgs.config.allowUnfree = true;
}
