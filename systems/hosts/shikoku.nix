{ config, lib, pkgs, ... }:

with lib;
let
  hostname = "shikoku";
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  ip = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";
  ips = lists.optionals secretCondition ([ "${ip}/24" ]);
  endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;
in
{
  imports = [
    # (import ../../nix).home-manager-stable
    #../modules/default.stable.nix
    (import ../../users).vincent
    (import ../../users).root
  ];

  networking = {
    bridges.br1.interfaces = [ "enp0s31f6" ];
    firewall.enable = false; # we are in safe territory :D
    useDHCP = false;
    interfaces.br1 = {
      useDHCP = true;
    };
  };

  boot.binfmt.registrations = {
    s390x-linux = {
      # interpreter = getEmulator "s390x-linux";
      interpreter = "${pkgs.qemu}/bin/qemu-s390x";
      magicOrExtension = ''\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x16'';
      mask = ''\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'';
    };
  };
  boot.binfmt.emulatedSystems = [
    "armv6l-linux"
    "armv7l-linux"
    "aarch64-linux"
    # "s390x-linux"
    "powerpc64le-linux"
  ];

  # TODO: check if it's done elsewhere
  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "nvme" "usb_storage" "usbhid" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # TODO: check if it's done elsewhere
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/73fd8864-f6af-4fdd-b826-0dfdeacd3c19";
      fsType = "ext4";
      #   options = [ "noatime" "discard" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/829D-BFD1";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/a9ec44e6-0c1d-4f60-9f5c-81a7eaa8e8fd"; }];

  profiles = {
    #home = true;
    dev.enable = lib.mkForce false;
    desktop.enable = lib.mkForce false;
    avahi.enable = true;
    syncthing.enable = true;
    ssh = { enable = true; };
    virtualization = { enable = true; nested = true; listenTCP = true; };
  };
  services = {
    netdata.enable = true;
    syncthing.guiAddress = "${ip}:8384";
    smartd = {
      enable = true;
      devices = [{ device = "/dev/nvme0n1"; }];
    };
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
  };

  # Move this to a "builder" role
  users.extraUsers.builder = {
    isNormalUser = true;
    uid = 1018;
    extraGroups = [ ];
    openssh.authorizedKeys.keys = [ (builtins.readFile ../../secrets/builder.pub) ];
  };
  nix.trustedUsers = [ "root" "vincent" "builder" ];
}
