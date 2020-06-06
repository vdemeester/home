{ lib, ... }:

with lib;
let
  hostname = "hokkaido";
  secretPath = ../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  ip = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";
  ips = lists.optionals secretCondition ([ "${ip}/24" ]);
  endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;
in
{
  imports = [
    ./hardware/thinkpad-x220.nix
    ./modules
    (import ../users).vincent
    (import ../users).root
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/884a3d57-f652-49b2-9c8b-f6eebd5edbeb";
    fsType = "ext4";
  };
  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/C036-34B9";
    fsType = "vfat";
  };
  swapDevices = [{ device = "/dev/disk/by-uuid/e1833693-77ac-4d52-bcc7-54d082788639"; }];

  networking = {
    hostName = hostname;
  };

  profiles = {
    home = true;
    laptop.enable = true;
    avahi.enable = true;
    git.enable = true;
    ssh.enable = true;
    dev.enable = true;
    yubikey.enable = true;
  };

  services = {
    fprintd.enable = true;
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
  };

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
