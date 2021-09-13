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

  lg_ultrawide_curved = "00ffffffffffff001e6df6760cd105000a1b010380502278eaca95a6554ea1260f50542108007140818081c0a9c0b300d1c081000101e77c70a0d0a0295030203a00204f3100001a9d6770a0d0a0225030203a00204f3100001a000000fd00383d1e5a20000a202020202020000000fc004c4720554c545241574944450a012902031ef12309070749100403011f13595a128301000067030c00100038409f3d70a0d0a0155030203a00204f3100001a7e4800e0a0381f4040403a00204f31000018011d007251d01e206e285500204f3100001e8c0ad08a20e02d10103e9600204f31000018000000ff003731304e544a4a42373139360a0000000000000033";
  thinkpadp1 = "00ffffffffffff000dae0c15000000002a1c0104b522137802ee95a3544c99260f505400000001010101010101010101010101010101363680a0703820405036680058c110000018363680a0703820405036680058c110000018000000fe00434d4e0a202020202020202020000000fe004e3135364843452d474e310a2001d102030f00e3058000e60605016a6a2400000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000005435313343363031415230320000000000000000000000000000000000de";
in
{
  imports = [
    ../hardware/lenovo-p1.nix
    (import ../../nix).home-manager-stable
    ../modules
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
    desktop.i3.enable = true;
    laptop.enable = true;
    home = true;
    dev.enable = true;
    yubikey.enable = true;
    virtualization = { enable = true; nested = true; };
    redhat.enable = true;
    ssh.enable = true;
  };

  # services.xserver.videoDrivers = [ "nvidia" ];
  # hardware.nvidia.prime.offload.enable = true;

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
        eDP-1 = thinkpadp1;
        DP-3-1 = lg_ultrawide_curved;
      };
      config = {
        eDP-1 = {
          enable = false;
        };
        DP-3-1 = {
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

}
