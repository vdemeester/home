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

  # Scripts
  officemode = pkgs.writeShellScriptBin "officemode" ''
    echo "80" > /sys/class/power_supply/BAT0/charge_control_end_threshold
    echo "70" > /sys/class/power_supply/BAT0/charge_control_start_threshold
  '';
  roadmode = pkgs.writeShellScriptBin "roadmode" ''
    echo "100" > /sys/class/power_supply/BAT0/charge_control_end_threshold
    echo "99" > /sys/class/power_supply/BAT0/charge_control_start_threshold
  '';
in
{
  imports = [
    ../hardware/thinkpad-x1g9.nix
    ../../users/vincent
    ../../users/root
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
      #device = "/dev/nvme0n1p1";
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
    kernelPackages = pkgs.linuxPackages_latest;
  };

  hardware.sensor.iio.enable = true;
  services.udev.extraRules = ''
    # STM32 rules for the Moonlander and Planck EZ
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", \
        MODE:="0666", \
        SYMLINK+="stm32_dfu"

    # Suspend the system when battery level drops to 5% or lower
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-5]", RUN+="${pkgs.systemd}/bin/systemctl hibernate"
  '';

  security.sudo.extraRules = [
    # Allow execution of roadmode and officemode by users in wheel, without a password
    {
      groups = [ "wheel" ];
      commands = [
        { command = "${officemode}/bin/officemode"; options = [ "NOPASSWD" ]; }
        { command = "${roadmode}/bin/roadmode"; options = [ "NOPASSWD" ]; }
      ];
    }
  ];

  modules = {
    editors.emacs.enable = true;
    hardware = {
      yubikey = { enable = true; u2f = true; };
      laptop.enable = true;
      bluetooth.enable = true;
    };
    desktop = {
      wayland.sway.enable = true;
    };
    dev = {
      enable = true;
      containers = {
        enable = true;
        # docker.enable = true;
        podman.enable = true;
      };
    };
    profiles = {
      #   home = true;
      work.redhat = true;
    };
    services = {
      syncthing = {
        enable = true;
        guiAddress = "${metadata.hosts.wakasu.wireguard.addrs.v4}:8384";
      };
      ssh.enable = true;
    };
  };

  # TODO Migrate to modules
  profiles.home = true;
  # Temporarly disable external builders, home network is down.
  # profiles.externalbuilder.enable = true;
  profiles.virtualization = { enable = true; nested = true; };
  virtualisation.podman.dockerSocket.enable = true;
  virtualisation.podman.autoPrune.enable = true;
  environment.systemPackages = with pkgs; [
    # docker client only
    (docker_24.override { clientOnly = true; })
    officemode
    roadmode
    obsidian
    discord
    virt-manager
  ];

  services = {
    geoclue2.enable = true;
    envfs.enable = false;
    # automatic login is "safe" as we ask for the encryption passphrase anyway..
    getty.autologinUser = "vincent";
    wireguard = {
      enable = true;
      ips = [ "${metadata.hosts.wakasu.wireguard.addrs.v4}/24" ];
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
  };

}
