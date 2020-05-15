{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  networking = {
    firewall.enable = false; # we are in safe territory :D
    # Move this to private/*.nix
    hosts = {
      "${home.ips.honshu}" = [ "honshu.home" ];
      "${wireguard.ips.honshu}" = [ "honshu.vpn" ];
      "${home.ips.shikoku}" = [ "shikoku.home" ];
      "${wireguard.ips.shikoku}" = [ "shikoku.vpn" ];
      "${home.ips.wakasu}" = [ "wakasu.home" ];
      "${wireguard.ips.wakasu}" = [ "wakasu.vpn" ];
      "${home.ips.hokkaido}" = [ "hokkaido.home" ];
      "${wireguard.ips.hokkaido}" = [ "hokkaido.vpn" ];
      "${home.ips.sakhalin}" = [ "sakhalin.home" ];
      "${wireguard.ips.sakhalin}" = [ "sakhalin.vpn" ];
      "${wireguard.ips.massimo}" = [ "massimo.vpn" ];
      "${home.ips.synodine}" = [ "synodine.home" ];
      "${home.ips.okinawa}" = [ "okinawa.home" "cache.home" "svc.home" "nix.cache.home" "go.cache.home" ];
      "${wireguard.ips.okinawa}" = [ "okinawa.vpn" ];
      "${wireguard.ips.carthage}" = [ "carthage.vpn" ];
      "${wireguard.ips.kerkouane}" = [ "kerkouane.vpn" ];
    };
    networkmanager = {
      dns = "dnsmasq";
    };
  };
  profiles = {
    dev.enable = true;
    nix-config.buildCores = 4;
    #qemu-user = { arm = true; aarch64 = true; };
    ssh = {
      enable = true;
      forwardX11 = true;
    };
  };
  programs = {
    podman.enable = true;
  };
  security.sudo = {
    extraConfig = ''
      %users ALL = (root) NOPASSWD: /home/vincent/.nix-profile/bin/kubernix
    '';
  };
}
