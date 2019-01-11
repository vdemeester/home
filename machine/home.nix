{ config, pkgs, ... }:

with import ../assets/machines.nix; {
  time.timeZone = "Europe/Paris";
  fileSystems."/mnt/synodine" = {
    device = "${home.ips.synodine}:/";
    fsType = "nfs";
    options = ["x-systemd.automount" "noauto"];
  };
}
