{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  time.timeZone = "Europe/Paris";
  profiles = {
    buildkit.enable = true;
    containerd.enable = true;
    desktop.enable = true;
    dev.enable = true;
    docker.enable = true;
    gaming.enable = true;
    ssh.enable = true;
    virtualization.enable = true;
  };
  boot = {
    loader.efi.canTouchEfiVariables = true;
    loader.grub.enable = true;
    loader.grub.version = 2;
    loader.grub.efiSupport = true;
    loader.efi.efiSysMountPoint = "/boot";
    loader.grub.devices = ["nodev"];
    loader.grub.extraEntries = ''
          menuentry "Windows" {
            insmod part_gpt
            insmod fat
            insmod search_fs_uuid
            insmod chain
            search --fs-uuid --no-floppy --set=root 122F-2055
            chainloader ($root)/Microsoft/Boot/bootmgfw.efi
          }
        '';
    loader.grub.useOSProber = true;
    kernelModules = [ "kvm_intel" ];
    kernelParams = [ "kvm_intel.nested=1" ];
  };
  hardware.bluetooth.enable = true;
  programs.podman = {
    enable = true;
  };
  networking = {
    firewall.allowedUDPPortRanges = [ { from = 6001; to = 6101; } ];
    firewall.allowedTCPPorts = [ 7946 9000 5000 ];
  };
  services = {
    syncthing-edge.guiAddress = "${wireguard.ips.shikoku}:8384";
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.shikoku}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
    xserver = {
      videoDrivers = [ "nvidia" ];
      displayManager = {
        sessionCommands = ''
          xrandr --output DP-2 --auto --left-of DP-4 --output DP-4 --auto &
        '';
      };
      dpi = 96;
    };
  };
}
