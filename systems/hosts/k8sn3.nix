{ pkgs, lib, ... }:

let
  hostname = "k8sn3";
  kubeMasterIP = "192.168.1.130";
in
{
  imports = [
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
  ];

  fileSystems."/" = {
    device = "/dev/disk/by-label/nixos";
    fsType = "ext4";
    autoResize = true;
  };

  boot.growPartition = true;
  boot.kernelParams = [ "console=ttyS0" ];
  boot.loader.grub.device = "/dev/vda";
  boot.loader.timeout = 0;

  networking = {
    hostName = hostname;
  };

  profiles = {
    ssh.enable = true;
    # systemd-boot doesn't with nixos-generators 🙃
    base.systemd-boot = false;
    kubernetes = {
      enable = true;
      master = {
        enable = true;
        ip = kubeMasterIP;
      };
    };
  };

  users.extraUsers.root.password = "";
}
