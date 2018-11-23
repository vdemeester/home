{ config, pkgs, ... }:

{
  imports = [ ../hardware/thinkpad-x220.nix ];

  profiles.laptop.enable = true;
  profiles.ssh.enable = true;
  profiles.dev.enable = true;
  profiles.yubikey.enable = true;

  time.timeZone = "Europe/Paris";

  environment.systemPackages = with pkgs; [
    nfs-utils
    sshfs
  ];
  
  programs.podman = {
    enable = true;
  };

  services.autofs = {
    enable = true;
    debug = false;
    autoMaster = let
      mapConfSsh = pkgs.writeText "auto.sshfs"  ''
      shikoku.local -fstype=fuse,allow_other :sshfs\#shikoku.local\:
      '';
      mapConf = pkgs.writeText "auto"  ''
      synodine -fstype=nfs,rw 192.168.12.19:/
      '';
    in ''
      /auto file:${mapConf}
      /auto/sshfs file:${mapConfSsh} uid=1000,gid=100,--timeout=30,--ghost
    '';
  };

  services.xserver.displayManager.slim.theme = pkgs.fetchurl {
    url = "https://github.com/vdemeester/slim-themes/raw/master/docker-key-theme-0.1.tar.xz";
    sha256 = "127893l1nzqya0g68k8841g5lm3hlnx7b3b3h06axvplc54a1jd8";
  };

  services.syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.hokkaido}:8384";
  services.wireguard = with import ../assets/wireguard.nix; {
    enable = true;
    ips = [ "${ips.hokkaido}/24" ];
    endpoint = main.endpointIP;
    endpointPort = main.listenPort;
    endpointPublicKey = kerkouane.publicKey;
  };
}
