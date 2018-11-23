{ config, pkgs, ... }:

{
  imports = [ ../hardware/thinkpad-x220.nix ];
  time.timeZone = "Europe/Paris";
  profiles = {
    dev.enable = true;
    laptop.enable = true;
    ssh.enable = true;
    yubikey.enable = true;
  };
  programs.podman = {
    enable = true;
  };
  services = {
    autofs = {
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
    syncthing-edge.guiAddress = with import ../assets/machines.nix; "${wireguard.ips.hokkaido}:8384";
    wireguard = with import ../assets/machines.nix; {
      enable = true;
      ips = [ "${wireguard.ips.hokkaido}/24" ];
      endpoint = wg.endpointIP;
      endpointPort = wg.listenPort;
      endpointPublicKey = wireguard.kerkouane.publicKey;
    };
  };
  environment.systemPackages = with pkgs; [
    nfs-utils
    sshfs
  ];
}
