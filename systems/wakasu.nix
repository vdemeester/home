{ lib, pkgs, ... }:
let
  inCi = builtins.pathExists /home/build;
  enableHome = !inCi;
in
{
  imports = [
    # hardware
    ../hardware/lenovo-p50.nix
    # modules
    ../modules
    # users
    (import ../users).vincent
    (import ../users).root
  ];

  networking = {
    hostName = "wakasu";
  };

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/49167ed2-8411-4fa3-94cf-2f3cce05c940";
      preLVM = true;
      allowDiscards = true;
      keyFile = "/dev/disk/by-id/usb-_USB_DISK_2.0_070D375D84327E87-0:0";
      keyFileOffset = 30992883712;
      keyFileSize = 4096;
      fallbackToPassword = true;
    };
  };

  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/c44cdfec-b567-4059-8e66-1be8fec6342a";
      fsType = "ext4";
      options = [ "noatime" "discard" ];
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/E974-AB5D";
      fsType = "vfat";
    };

  swapDevices =
    [
      { device = "/dev/disk/by-uuid/c8c3308a-6ca6-4669-bad3-37a225af4083"; }
    ];

  profiles = {
    dev.enable = true;
    desktop.autoLogin = true;
    docker.enable = true;
    laptop.enable = true;
    ssh = { enable = true; forwardX11 = true; };
    virtualization = { enable = true; nested = true; listenTCP = true; };
    yubikey.enable = true;
  };
  programs = {
    podman.enable = true;
    crc.enable = true;
  };
  security = {
    sudo.extraConfig = ''
      %users ALL = (root) NOPASSWD: /home/vincent/.nix-profile/bin/kubernix
    '';
    pam.u2f.enable = true;
  };
  services = {
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    #syncthing.guiAddress = "${wireguard.ips.wakasu}:8384";
    syncthing.guiAddress = "0.0.0.0:8384";
    smartd = {
      enable = true;
      devices = [{ device = "/dev/nvme0n1"; }];
    };
    # FIXME handle secrets
    /*
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.wakasu}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
    */
    xserver = {
      videoDrivers = [ "nvidia" ];
      dpi = 96;
      serverFlagsSection = ''
        Option "BlankTime" "0"
        Option "StandbyTime" "0"
        Option "SuspendTime" "0"
        Option "OffTime" "0"
      '';
    };
  };

}
