{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  imports = [ ../hardware/lenovo-p50.nix ./home.nix ];
  boot = {
    kernelModules = [ "kvm_intel" ];
    kernelParams = [ "kvm_intel.nested=1" ];
    kernel.sysctl = {
      "net.bridge.bridge-nf-call-arptables" = 0;
      "net.bridge.bridge-nf-call-iptables" = 0;
      "net.bridge.bridge-nf-call-ip6tables" = 0;
    };
  };
  profiles = {
    dev.enable = true;
    docker.enable = true;
    #ipfs.enable = true;
    laptop.enable = true;
    desktop.autoLogin = true;
    nix-config.buildCores = 4;
    qemu-user = { arm = true; aarch64 = true; };
    ssh.enable = true;
    virtualization = {
      enable = true;
      listenTCP = true;
    };
  };
  programs = {
    podman.enable = true;
  };
  networking = {
    firewall.enable = false; # we are in safe territory :D
    bridges.br1.interfaces = [ "enp0s31f6" ];
    interfaces.enp0s31f6 = {
      useDHCP = true;
    };
    networkmanager.unmanaged = [ "interface-name:enp0s31f6" ];
  };
  services = {
    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandleLidSwitchExternalPower=ignore
      HandleLidSwitchDocked=ignore
    '';
    syncthing-edge.guiAddress = "${wireguard.ips.wakasu}:8384";
    smartd = {
      enable = true;
      devices = [ { device = "/dev/nvme0n1"; } ];
    };
    wireguard = {
      enable = true;
      ips = [ "${wireguard.ips.wakasu}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
    xserver = {
      videoDrivers = [ "nvidia" ];
      dpi = 96;
    };
  };
}
