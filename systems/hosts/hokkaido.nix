{ pkgs, lib, ... }:

with lib;
let
  hostname = "hokkaido";
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
    ../hardware/dell-latitude-e6540.nix
    ../modules
    (import ../../users).vincent
    (import ../../users).root
  ];
  fileSystems."/" =
    {
      device = "/dev/disk/by-uuid/3e86d004-5554-4a90-b436-fcca63775f9d";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    {
      device = "/dev/disk/by-uuid/D91F-14E8";
      fsType = "vfat";
    };

  swapDevices =
    [{ device = "/dev/disk/by-uuid/f065180d-8889-45ba-81d1-a67ac746dfeb"; }];

  networking = {
    hostName = hostname;
  };

  boot = {
    tmpOnTmpfs = true;
    plymouth.enable = true;
  };

  services.hardware.bolt.enable = true;
  profiles = {
    desktop.gnome.enable = true;
    laptop.enable = true;
    home = true;
    ssh.enable = true;
    dev.enable = true;
    yubikey.enable = true;
    virtualization = { enable = true; nested = true; };
    docker.enable = true;
    redhat.enable = true;
  };
  environment.systemPackages = with pkgs; [ virtmanager ];

  services = {
    # FIXME re-generate hokkaido key
    /*
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
    */
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
