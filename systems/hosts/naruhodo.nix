{ sources ? import ../../nix
, lib ? sources.lib
, pkgs ? sources.pkgs { }
, ...
}:

with lib;
let
  hostname = "naruhodo";
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
    ../hardware/thinkpad-t480s.nix
    (import ../../nix).home-manager
    ../modules
    (import ../../users).vincent
    (import ../../users).root
  ];

  fileSystems."/" = {
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

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/0101-68DE";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-uuid/aff86817-55ae-47ed-876a-e5a027b560ba"; }];

  networking = {
    hostName = hostname;
  };

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    tmpOnTmpfs = true;
    plymouth = {
      enable = true;
      themePackages = [ pkgs.my.adi1090x-plymouth ];
      theme = "cuts";
      # hexagon, green_loader, deus_ex, cuts, sphere, spinner_alt
    };
    extraModulePackages = with pkgs.linuxPackages_latest; [
      v4l2loopback
    ];
    kernelModules = [ "v4l2loopback" ];
    extraModprobeConfig = ''
      options v4l2loopback exclusive_caps=1
    '';
    binfmt.registrations = {
      s390x-linux = {
        # interpreter = getEmulator "s390x-linux";
        interpreter = "${pkgs.qemu}/bin/qemu-s390x";
        magicOrExtension = ''\x7fELF\x02\x02\x01\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x16'';
        mask = ''\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xfe\xff\xff'';
      };
    };
    binfmt.emulatedSystems = [
      "armv6l-linux"
      "armv7l-linux"
      "aarch64-linux"
      # "s390x-linux"
      "powerpc64le-linux"
    ];
  };

  # FIXME Fix tmpOnTmpfs
  systemd.additionalUpstreamSystemUnits = [ "tmp.mount" ];

  # nix.distributedBuilds = true;
  nix.buildMachines = [
    {
      hostName = "192.168.1.77";
      maxJobs = 8;
      sshUser = "builder";
      sshKey = "/etc/nixos/secrets/builder";
      systems = [ "x86_64-linux" "aarch64-linux" "armv7l-linux" "armv6l-linux" "powerpc64le-linux" "s390x-linux" ];
      supportedFeatures = [
        "big-parallel"
        "kvm"
        "nixos-test"
      ];
    }
    # {
    #   hostName = "192.168.1.115";
    #   maxJobs = 8;
    #   sshUser = "builder";
    #   sshKey = "/etc/nixos/secrets/builder";
    #   systems = [ "x86_64-linux" "aarch64-linux" "armv7l-linux" "armv6l-linux" "powerpc64le-linux" "s390x-linux" ];
    #   supportedFeatures = [
    #     "big-parallel"
    #     "kvm"
    #     "nixos-test"
    #   ];
    # }
  ];

  programs.ssh.knownHosts = {
    "wakasu" = {
      hostNames = [ "wakasu.home" "192.168.1.77" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJ2GB030S1+iZMqwgYhkl5CuBOKBjZoujc0aVHII39/x";
    };
    "hokkaido" = {
      hostNames = [ "hokkaido.home" "192.168.1.115" ];
      publicKey = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB5hoyiE7fj+/vUzvvFD2r2Mm4p86p6uPDOp0ChzR5ZC";
    };
  };

  services.udev.extraRules = ''
    # Teensy rules for the Ergodox EZ
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", ENV{ID_MM_DEVICE_IGNORE}="1"
    ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789A]?", ENV{MTP_NO_PROBE}="1"
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789ABCD]?", MODE:="0666"
    KERNEL=="ttyACM*", ATTRS{idVendor}=="16c0", ATTRS{idProduct}=="04[789B]?", MODE:="0666"

    # STM32 rules for the Moonlander and Planck EZ
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", \
        MODE:="0666", \
        SYMLINK+="stm32_dfu"

    # Suspend the system when battery level drops to 5% or lower
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-5]", RUN+="${pkgs.systemd}/bin/systemctl hibernate"
  '';
  services.hardware.bolt.enable = true;
  core.nix = {
    # temporary
    localCaches = [ ];
  };

  profiles = {
    desktop.i3.enable = true;
    laptop.enable = true;
    home = true;
    dev.enable = true;
    yubikey.enable = true;
    virtualization = { enable = true; nested = true; };
    redhat.enable = true;
    scanning.enable = true;
    ssh.enable = true;
  };
  environment.systemPackages = with pkgs; [
    virtmanager
    # force xbacklight to work
    acpilight
  ];

  # warnings = "metadata… ${metadata.hosts.naruhodo.wireguard.addrs.v4}";
  services = {
    wireguard = {
      enable = true;
      ips = [ "${metadata.hosts.naruhodo.wireguard.addrs.v4}/24" ];
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
    syncthing.guiAddress = "${metadata.hosts.naruhodo.wireguard.addrs.v4}:8384";
  };
  systemd.services.buildkitd.wantedBy = lib.mkForce [ ];
  systemd.services.containerd.wantedBy = lib.mkForce [ ];
  systemd.services.docker.wantedBy = lib.mkForce [ ];
  systemd.services.docker.requires = [ "containerd.socket" ];

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
