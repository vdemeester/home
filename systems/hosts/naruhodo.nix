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

  ip = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";
  ips = lists.optionals secretCondition ([ "${ip}/24" ]);
  endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;

  getEmulator = system: (lib.systems.elaborate { inherit system; }).emulator pkgs;

  lg_ultrawide_curved = "00ffffffffffff001e6df6760cd105000a1b010380502278eaca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a012902031ef12309070749100403011f13595a128301000067030c00100038409f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000ff003731304e544a4a42373139360a0000000000000033";
  thinkpadt480s = "00ffffffffffff000daec91400000000081a0104951f11780228659759548e271e505400000001010101010101010101010101010101b43b804a71383440503c680035ad10000018000000fe004e3134304843412d4541420a20000000fe00434d4e0a202020202020202020000000fe004e3134304843412d4541420a20003e";
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
    docker.enable = true;
    redhat.enable = true;
    scanning.enable = true;
  };
  environment.systemPackages = with pkgs; [
    virtmanager
    # force xbacklight to work
    acpilight
  ];

  programs.autorandr.profiles = {
    on-the-move = {
      fingerprint = {
        eDP-1 = thinkpadt480s;
      };
      config = {
        eDP-1 = {
          enable = true;
          primary = true;
          position = "0x0";
          mode = "1920x1080";
        };
      };
    };
    home = {
      fingerprint = {
        eDP-1 = thinkpadt480s;
        DP-1-1 = lg_ultrawide_curved;
      };
      config = {
        eDP-1 = {
          enable = false;
        };
        DP-1-1 = {
          enable = true;
          primary = true;
          mode = "3440x1440";
          position = "0x0";
        };
      };
    };
  };

  services = {
    wireguard = {
      enable = true;
      ips = ips;
      endpoint = endpointIP;
      endpointPort = endpointPort;
      endpointPublicKey = endpointPublicKey;
    };
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
