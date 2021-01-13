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
    (import ../../nix).home-manager-stable
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
    bridges.br1.interfaces = [ "eno1" ];
    firewall.enable = false; # we are in safe territory :D
    useDHCP = false;
    interfaces.br1 = {
      useDHCP = true;
    };
  };

  boot = {
    tmpOnTmpfs = true;
    plymouth.enable = true;
  };

  # FIXME Fix tmpOnTmpfs
  systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];

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

  users.extraUsers.builder = {
    isNormalUser = true;
    uid = 1018;
    extraGroups = [ ];
    openssh.authorizedKeys.keys = [ (builtins.readFile "/etc/nixos/secrets/builder.pub") ];
  };
  nix.trustedUsers = [ "root" "vincent" "builder" ];

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

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
  };

}
