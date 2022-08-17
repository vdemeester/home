# { sources ? import ../../nix
# , lib ? sources.lib
# , pkgs ? sources.pkgs { }
# , ...
# }:
{ config, lib, pkgs, ... }:

with lib;
let
  hostname = "wakasu";
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;

  getEmulator = system: (lib.systems.elaborate { inherit system; }).emulator pkgs;
  metadata = importTOML ../../ops/hosts.toml;
in
{
  imports = [
    ../hardware/thinkpad-x1g9.nix
    (import ../../users).vincent
    (import ../../users).root
  ];

  fileSystems."/" = {
    device = "/dev/mapper/root";
    # uuid: 637ee2a5-638d-46cd-8845-3cc0fa8551bd
    fsType = "ext4";
    options = [ "noatime" "discard" ];
  };

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/c0cac87c-53ec-4262-9ab2-a3ee8331c75a";
      preLVM = true;
      allowDiscards = true;
      keyFile = "/dev/disk/by-id/usb-_USB_DISK_2.0_070D375D84327E87-0:0";
      keyFileOffset = 30992883712;
      keyFileSize = 4096;
      fallbackToPassword = true;
    };
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/7D17-F310";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-uuid/ab056cfc-fb17-4db7-a393-f93726cc2987"; }];

  networking = {
    hostName = hostname;
  };

  boot = {
    loader.systemd-boot.netbootxyz.enable = true;
    kernelPackages = pkgs.linuxPackages_latest;
    tmpOnTmpfs = true;
    plymouth = {
      enable = true;
      themePackages = [ pkgs.my.adi1090x-plymouth ];
      theme = "deus_ex";
      # hexagon, green_loader, deus_ex, cuts, sphere, spinner_alt
    };
  };

  # FIXME Fix tmpOnTmpfs
  systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];


  services.udev.extraRules = ''
    # STM32 rules for the Moonlander and Planck EZ
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", \
        MODE:="0666", \
        SYMLINK+="stm32_dfu"

    # Suspend the system when battery level drops to 5% or lower
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-5]", RUN+="${pkgs.systemd}/bin/systemctl hibernate"
  '';
  services.hardware.bolt.enable = true;
  core.nix = {
    # temporary or not
    localCaches = [ ];
  };

  modules = {
    hardware = {
      #yubikey.enable = true;
    };
  };

  profiles.ssh.enable = true;

  environment.systemPackages = with pkgs; [
    docker-client
  ];

  services = {
    logind.extraConfig = ''
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    wireguard = {
      enable = true;
      ips = [ "${metadata.hosts.naruhodo.wireguard.addrs.v4}/24" ];
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
    # syncthing.guiAddress = "${metadata.hosts.naruhodo.wireguard.addrs.v4}:8384";
  };

}
