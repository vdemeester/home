{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ./home.nix ];
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
  networking = {
    firewall.allowedUDPPortRanges = [ { from = 6001; to = 6101; } ];
    firewall.allowedTCPPorts = [ 7946 9000 5000 ];
  };
  profiles = {
    buildkit.enable = true;
    desktop.enable = true;
    dev.enable = true;
    docker.enable = true;
    gaming.enable = true;
    nix-config.buildCores = 4;
    ssh.enable = true;
    virtualization.enable = true;
  };
  programs.podman = {
    enable = true;
  };
  services = {
    syncthing-edge.guiAddress = "${wireguard.ips.shikoku}:8384";
    smartd.enable = true;
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.shikoku}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
    xserver = {
      videoDrivers = [ "nvidia" ];
      xrandrHeads = [
        { output = "HDMI-0"; primary = true; }
        { output = "DP-0"; monitorConfig = "Option \"Rotate\" \"right\""; }
      ];
      dpi = 96;
    };
  };
}
