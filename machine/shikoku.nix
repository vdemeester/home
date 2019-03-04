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
    kernel.sysctl = {
      "net.bridge.bridge-nf-call-arptables" = 0;
      "net.bridge.bridge-nf-call-iptables" = 0;
      "net.bridge.bridge-nf-call-ip6tables" = 0;
    };
  };
  hardware.bluetooth.enable = true;
  networking = {
    firewall.enable = false; # we are in safe territory :D
    bridges.br1.interfaces = [ "enp0s31f6" ];
    interfaces.enp0s31f6 = {
      useDHCP = true;
    };
  };
  profiles = {
    buildkit.enable = true;
    desktop = {
      enable = true;
      networkmanager = false;
    };
    dev.enable = true;
    docker.enable = true;
    gaming.enable = true;
    #ipfs.enable = true;
    nix-config.buildCores = 4;
    qemu-user = { arm = true; aarch64 = true; };
    ssh.enable = true;
    virtualization = {
      enable = true;
      listenTCP = true;
    };
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
