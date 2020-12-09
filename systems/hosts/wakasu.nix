{ lib, pkgs, ... }:

with lib;
let
  hostname = "wakasu";
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
    ../hardware/lenovo-p50.nix
    ../modules
    (import ../../users).vincent
    (import ../../users).root
  ];

  networking = {
    hostName = hostname;
    bridges.br1.interfaces = [ "enp0s31f6" ];
    firewall.enable = false; # we are in safe territory :D
    useDHCP = false;
    interfaces.br1 = {
      useDHCP = true;
    };
  };

  /*
  Keep this for naruhodo.
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
  */
  fileSystems."/" = {
    device = "/dev/disk/by-uuid/6590b73d-72a4-4356-94b1-f56ac45c976d";
    fsType = "ext4";
    options = [ "noatime" "discard" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/7FA5-145B";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-uuid/720200fc-8f27-49a7-85bb-a406b6119d31"; }];

  profiles = {
    home = true;
    dev.enable = true;
    desktop.enable = lib.mkForce false;
    laptop.enable = true;
    docker.enable = true;
    avahi.enable = true;
    syncthing.enable = true;
    ssh = { enable = true; forwardX11 = true; };
    virtualization = { enable = true; nested = true; listenTCP = true; };
    kubernetes.enable = true;
    openshift.enable = true;
    tekton.enable = true;
    yubikey.enable = true;
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
  security = {
    sudo.extraConfig = ''
      %users ALL = (root) NOPASSWD: /home/vincent/.nix-profile/bin/kubernix
    '';
    pam.u2f.enable = true;
  };
  services = {
    xserver = {
      enable = true;
      displayManager.xpra = {
        enable = true;
        bindTcp = "0.0.0.0:10000";
        pulseaudio = true;
        extraOptions = [ "--video-scaling=0" "--min-quality=85" "--desktop-scaling=off" ];
      };
    };
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
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
  };
  /*
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
  */
}
