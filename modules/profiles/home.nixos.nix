{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.home;
  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);
  machines = lib.optionalAttrs secretCondition (import secretPath);
in
{
  options = {
    profiles.home = mkEnableOption "Enable home profile";
  };
  config = mkIf cfg {
    boot.kernelParams = [ "nfs.nfs4_disable_idmapping=0" "nfsd.nfs4_disable_idmapping=0" ];
    networking.domain = "home";
    time.timeZone = "Europe/Paris";
    # To mimic autofs on fedora
    fileSystems = mkIf secretCondition {
      "/net/synodine.home/" = {
        device = "${machines.home.ips.synodine}:/";
        fsType = "nfs";
        options = [ "x-systemd.automount" "noauto" ];
      };
      # FIXME(vdemeester): I think it acts like this because there is only one export
      "/net/sakhalin.home/export/" = {
        device = "${machines.home.ips.sakhalin}:/";
        fsType = "nfs";
        options = [ "x-systemd.automount" "noauto" ];
      };
      # Deprecated
      "/mnt/synodine" = {
        device = "${machines.home.ips.synodine}:/";
        fsType = "nfs";
        options = [ "x-systemd.automount" "noauto" ];
      };
      "/mnt/sakhalin" = {
        device = "${machines.home.ips.sakhalin}:/";
        fsType = "nfs";
        options = [ "x-systemd.automount" "noauto" ];
      };
    };
  };
}
