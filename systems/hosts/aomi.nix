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
    (import ../../users/vincent)
    (import ../../users/root)
  ];

  # FILESYSTEM
  boot.initrd.luks.devices = {
    root = {
      device = "/dev/disk/by-uuid/91b05f64-b97d-4405-8405-8785699ada8f";
      preLVM = true;
      allowDiscards = true;
      keyFile = "/dev/disk/by-id/mmc-SD08G_0x704a5a38";
      keyFileSize = 4096;
      fallbackToPassword = true;
    };
  };

  fileSystems."/" = {
    # device = "/dev/disk/by-uuid/6bedd234-3179-46f7-9a3f-feeffd880791";
    device = "/dev/mapper/root";
    fsType = "ext4";
    options = [ "noatime" "discard" ];
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/32B9-94CC";
    fsType = "vfat";
  };

  swapDevices = [{ device = "/dev/disk/by-uuid/24da6a46-cd28-4bff-9220-6f449e3bd8b5"; }];

  networking = {
    hostName = hostname;
    firewall.enable = false; # we are in safe territory :D
  };

  # modules = {
  #   desktop.enable = true; # defaults to wayland
  #   hardware = {
  #     laptop = true; # infers bluetooth and yubikey
  #   };
  #   dev = {
  #     enable = true; # infers containers by default, one would have to disable it
  #     containers = {
  #       buildkit = {
  #         enable = true;
  #         grpcAddress = [
  #           "unix:///run/buildkit/buildkitd.sock"
  #           "tcp://aomi.home:1234"
  #           "tcp://${metadata.hosts.aomi.addrs.v4}:1234"
  #           "tcp://${metadata.hosts.aomi.wireguard.addrs.v4}:1234"
  #         ];
  #       };
  #       image-mirroring = {
  #         enable = true;
  #         targets = [ "quay.io/vdemeest" "ghcr.io/vdemeester" ];
  #         settings = {
  #           "docker.io" = {
  #             "images" = {
  #               # sync latest and edge tags
  #               "alpine" = [ "latest" "edge" ];
  #             };
  #             "images-by-tag-regex" = {
  #               # sync all "3.x" images"
  #               "alpine" = "^3\.[0-9]+$";
  #             };
  #           };
  #         };
  #       };
  #     };
  #     profiles = {
  #       home = true; # with laptop, infers avahi
  #       work.redhat = true; # rename this probably
  #     };
  #     services = {
  #       ssh.enable = true;
  #       # syncthing is inferred
  #     };
  #   };
  # };

  # Below this line, migrate

  # extract this from desktop
  networking.networkmanager = {
    enable = true;
    unmanaged = [
      "interface-name:br-*"
      "interface-name:ve-*"
      "interface-name:veth*"
      "interface-name:wg0"
      "interface-name:docker0"
      "interface-name:virbr*"
    ];
    packages = with pkgs; [ networkmanager-openvpn ];
  };

  boot = {
    loader.systemd-boot.netbootxyz.enable = true;
    kernelPackages = pkgs.linuxPackages_latest;
    tmpOnTmpfs = true;
  };

  services.hardware.bolt.enable = true;

  modules = {
    core.binfmt.enable = true;
    editors.emacs.enable = true;
    editors.neovim.enable = true;
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
        docker = {
          enable = true;
          package = pkgs.docker_27;
        };
        podman.enable = true;
        buildkit = {
          enable = true;
          grpcAddress = [
            "unix:///run/buildkit/buildkitd.sock"
            "tcp://aomi.home:1234"
            "tcp://${metadata.hosts.aomi.addrs.v4}:1234"
            "tcp://${metadata.hosts.aomi.wireguard.addrs.v4}:1234"
          ];
        };
        image-mirroring = {
          enable = true;
          targets = [ "quay.io/vdemeest" "ghcr.io/vdemeester" ];
          settings = {
            "docker.io" = {
              "images" = {
                # sync latest and edge tags
                "alpine" = [ "latest" "edge" ];
              };
              "images-by-tag-regex" = {
                # sync all "3.x" images"
                "alpine" = "^3\.[0-9]+$";
              };
            };
          };
        };
      };
    };
    profiles = {
      work.redhat = true;
    };
    services = {
      avahi.enable = true;
      ssh.enable = true;
      syncthing = {
        enable = true;
        guiAddress = "${metadata.hosts.aomi.wireguard.addrs.v4}:8384";
      };
    };
    virtualisation.libvirt = { enable = true; nested = true; };
  };

  modules.profiles = {
    # externalbuilder.enable = true;
    home = true;
  };

  environment.systemPackages = with pkgs; [
    virt-manager
    catt
    go-org-readwise
    vscode
    # move to its own
    passage
    age
    age-plugin-yubikey
    age-plugin-tpm
  ];

  services.udev.extraRules = ''
    # STM32 rules for the Moonlander and Planck EZ
    SUBSYSTEMS=="usb", ATTRS{idVendor}=="0483", ATTRS{idProduct}=="df11", \
        MODE:="0666", \
        SYMLINK+="stm32_dfu"

    # Suspend the system when battery level drops to 5% or lower
    SUBSYSTEM=="power_supply", ATTR{status}=="Discharging", ATTR{capacity}=="[0-5]", RUN+="${pkgs.systemd}/bin/systemctl hibernate"
  '';

  services = {
    ollama = {
      enable = true;
      # acceleration = "cuda"; # no nivida :D
    };
    geoclue2.enable = true;
    # automatic login is "safe" as we ask for the encryption passphrase anyway..
    getty.autologinUser = "vincent";
    logind.extraConfig = ''
      HandleLidSwitch=ignore
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    prometheus.exporters.node = {
      enable = true;
      port = 9000;
      enabledCollectors = [ "systemd" "processes" ];
      extraFlags = [ "--collector.ethtool" "--collector.softirqs" "--collector.tcpstat" ];
    };
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

  # Move this to a "builder" role
  users.extraUsers.builder = {
    isNormalUser = true;
    uid = 1018;
    extraGroups = [ ];
    openssh.authorizedKeys.keys = [ (builtins.readFile ../../secrets/builder.pub) ];
  };
  nix.trustedUsers = [ "root" "vincent" "builder" ];

  security = {
    tpm2 = {
      enable = true;
      pkcs11.enable = true;
    };
    pam.enableSSHAgentAuth = true;
  };
}
