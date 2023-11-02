{ sources ? import ../../nix
, lib ? sources.lib
, pkgs ? sources.pkgs { }
, ...
}:

let
  hostname = "k8sn2.home";
  kubeMasterIP = "192.168.1.130";
in
{
  imports = [
    <nixpkgs/nixos/modules/profiles/qemu-guest.nix>
    (import ../../nix).home-manager-stable
    ../modules/default.stable.nix
    # FIXME Need to refactor vincent user as.. it's adding way to much by default...

   import ../../users/root
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
    domain = "home";
    firewall.enable = false;
  };

  profiles = {
    nix-auto-update.enable = false;
    ssh.enable = true;
    # systemd-boot doesn't with nixos-generators 🙃
    base.systemd-boot = false;
    kubernetes = {
      enable = true;
      master = {
        enable = false;
        ip = kubeMasterIP;
      };
    };
  };

  users.extraUsers.root.password = "";
}
