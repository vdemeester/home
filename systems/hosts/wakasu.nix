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
    (import ../../nix).home-manager-stable
    ../modules/default.stable.nix
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
    bind.enable = true;
    home = true;
    dev.enable = false;
    desktop.enable = lib.mkForce false;
    laptop.enable = true;
    docker.enable = true;
    avahi.enable = true;
    syncthing.enable = true;
    ssh = { enable = true; };
    virtualization = { enable = true; nested = true; listenTCP = true; };
  };
  security = {
    pam.u2f.enable = true;
  };
  services = {
    netdata.enable = true;
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
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
    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      virtualHosts."whoami.sbr.pm" = {
        locations."/" = {
          proxyPass = "http://192.168.1.187:80";
          extraConfig = ''
            proxy_set_header Host            $host;
            proxy_set_header X-Forwarded-For $remote_addr;
          '';
        };
      };
      virtualHosts."webhook.sbr.pm" = {
        # listen = [{ port = 8080; }];
        locations."/" = {
          proxyPass = "http://192.168.1.188:8080";
          extraConfig = ''
            proxy_set_header Host            $host;
            proxy_set_header X-Forwarded-For $remote_addr;
          '';
        };
      };
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
