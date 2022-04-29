{ sources ? import ../../nix
, lib ? sources.lib
, pkgs ? sources.pkgs { }
, ...
}:

with lib;
let
  hostname = "aomi";
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);

  ip = strings.optionalString secretCondition (import secretPath).wireguard.ips."${hostname}";
  ips = lists.optionals secretCondition ([ "${ip}/24" ]);
  endpointIP = strings.optionalString secretCondition (import secretPath).wg.endpointIP;
  endpointPort = if secretCondition then (import secretPath).wg.listenPort else 0;
  endpointPublicKey = strings.optionalString secretCondition (import secretPath).wireguard.kerkouane.publicKey;
  metadata = importTOML ../../ops/hosts.toml;
in
{
  imports = [
    ../hardware/lenovo-p1.nix
    #(import ../../nix).home-manager
    #../modules
    (import ../../users).vincent
    (import ../../users).root
  ];

  fileSystems."/" = {
    device = "/dev/vg/root";
    fsType = "ext4";
    options = [ "noatime" "discard" ];
  };

  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/9d53a7f3-b75d-4121-b6c3-4b4c4a33ee52";
      preLVM = true;
      allowDiscards = true;
    };
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/53E3-FA1E";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-uuid/9525a5eb-1e57-47bf-9c62-1caf466cf3be"; }];

  networking = {
    hostName = hostname;
  };

  sops.defaultSopsFile = ../../secrets/secrets.yaml;

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;
    tmpOnTmpfs = true;
    plymouth = {
      enable = true;
      themePackages = [ pkgs.my.adi1090x-plymouth ];
      theme = "hexagon";
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

  services.hardware.bolt.enable = true;

  profiles = {
    externalbuilder.enable = true;
    desktop.i3.enable = true;
    laptop.enable = true;
    home = true;
    dev.enable = true;
    yubikey.enable = true;
    virtualization = { enable = true; nested = true; };
    redhat.enable = true;
    ssh.enable = true;
    docker.enable = true;
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

    # Allow brightness
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chgrp video /sys/class/backlight/%k/brightness"
    ACTION=="add", SUBSYSTEM=="backlight", KERNEL=="intel_backlight", RUN+="${pkgs.coreutils}/bin/chmod g+w /sys/class/backlight/%k/brightness"
  '';

  environment.systemPackages = with pkgs; [
    virtmanager
    # force xbacklight to work
    acpilight
    brightnessctl
    steam-run
    asciinema
  ];

  services = {
    syncthing.guiAddress = "${metadata.hosts.aomi.wireguard.addrs.v4}:8384";
    smartd = {
      enable = true;
      devices = [{ device = "/dev/nvme0n1"; }];
    };
    wireguard = {
      enable = true;
      ips = [ "${metadata.hosts.aomi.wireguard.addrs.v4}/24" ];
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

  # Move this to a "builder" role
  users.extraUsers.builder = {
    isNormalUser = true;
    uid = 1018;
    extraGroups = [ ];
    openssh.authorizedKeys.keys = [ (builtins.readFile ../../secrets/builder.pub) ];
  };
  nix.trustedUsers = [ "root" "vincent" "builder" ];

  # RedHat specific
  systemd.services.osp-vdemeest-nightly = {
    description = "Build nightly builds";
    requires = [ "network-online.target" ];
    after = [ "network-online.target" ];

    restartIfChanged = false;
    unitConfig.X-StopOnRemoval = false;

    serviceConfig = {
      Type = "oneshot";
      User = "vincent";
      OnFailure = "status-email-root@%.service";
    };

    path = with pkgs; [ git openssh gnumake docker-client bash coreutils-full gawk curl nix kustomize which ];
    script = ''
      set -e
      cd /home/vincent/src/osp/p12n/p12n
      git fetch -p --all
      BRANCH="git symbolic-ref --short HEAD"
      # 1.8
      git checkout upstream/pipelines-1.8-rhel-8
      nix-shell /home/vincent/src/osp/shell.nix --command 'make REMOTE=quay.io/vdemeest TAG=1.8 sources/upgrade sources/operator/fetch-payload  bundle/push'
      # make REMOTE=quay.io/vdemeest TAG=1.8 sources/upgrade sources/operator/fetch-payload  bundle/push
      git reset --hard HEAD
      #1.7
      git checkout upstream/pipelines-1.7-rhel-8
      nix-shell /home/vincent/src/osp/shell.nix --command 'make REMOTE=quay.io/vdemeest TAG=1.7 sources/upgrade sources/operator/fetch-payload  bundle/push'
      # make REMOTE=quay.io/vdemeest TAG=1.7 sources/upgrade sources/operator/fetch-payload  bundle/push
      git reset --hard HEAD
      # revert
      git checkout $BRANCH
    '';

    startAt = "daily";
  };
}
