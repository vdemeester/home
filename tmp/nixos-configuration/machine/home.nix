{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  boot.kernelParams = [ "nfs.nfs4_disable_idmapping=0" "nfsd.nfs4_disable_idmapping=0" ];
  networking.domain = "synodine.home";
  time.timeZone = "Europe/Paris";
  fileSystems."/mnt/synodine" = {
    device = "${home.ips.synodine}:/";
    fsType = "nfs";
    options = ["x-systemd.automount" "noauto"];
  };
}
