{ pkgs, lib, ... }:

with lib;
let
  hostname = "naruhodo";
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
    ../hardware/thinkpad-t480s.nix
    (import ../../nix).home-manager
    ../modules
    (import ../../users).vincent
    (import ../../users).root
  ];

  fileSystems."/" =
    {
      device = "/dev/mapper/root";
      fsType = "ext4";
      options = [ "noatime" "discard" ];
    };

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/50d7faba-8923-4b30-88f7-40df26e02def";
      preLVM = true;
      allowDiscards = true;
    };
  };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/0101-68DE";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/aff86817-55ae-47ed-876a-e5a027b560ba"; }];

  networking = {
    hostName = hostname;
  };

  boot = {
    tmpOnTmpfs = true;
    plymouth.enable = true;
    extraModulePackages = with pkgs.linuxPackages; [
      v4l2loopback
    ];
    kernelModules = [ "v4l2loopback" ];
    extraModprobeConfig = ''
      options v4l2loopback exclusive_caps=1
    '';
    binfmt.emulatedSystems = [ "armv7l-linux" "aarch64-linux" ];
  };

  services.hardware.bolt.enable = true;
  profiles = {
    desktop.i3.enable = true;
    laptop.enable = true;
    home = true;
    dev.enable = true;
    yubikey.enable = true;
    virtualization = { enable = true; nested = true; };
    docker.enable = true;
    redhat.enable = true;
    scanning.enable = true;
  };
  environment.systemPackages = with pkgs; [ virtmanager ];

  services = {
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
  };

  virtualisation.podman.enable = true;
  virtualisation.containers = {
    enable = true;
    registries = {
      search = [ "registry.fedoraproject.org" "registry.access.redhat.com" "registry.centos.org" "docker.io" "quay.io" ];
    };
    policy = {
      default = [{ type = "insecureAcceptAnything"; }];
      transports = {
        docker-daemon = {
          "" = [{ type = "insecureAcceptAnything"; }];
        };
      };
    };
  };
}
