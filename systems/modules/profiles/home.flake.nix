{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption;
  cfg = config.modules.home;

  secretPath = ../../secrets/machines.nix;
  secretCondition = (builtins.pathExists secretPath);
  machines = lib.optionalAttrs secretCondition (import secretPath);
in
{
  options = {
    modules.home = {
      enable = mkEnableOption "home configuration";
    };
  };
  config = mkIf cfg.enable {

    # Mount nfs on all systems at home…
    # … if we got the secret file
    fileSystems = mkIf secretCondition {
      "/net/synodine.home/" = {
        device = "${machines.home.ips.synodine}:/";
        fsType = "nfs";
        options = [ "x-systemd.automount" "noauto" ];
      } // mkIf (config.networking.hostName != "sakhalin") {
        "/net/sakhalin.home/export/" = {
          device = "${machines.home.ips.sakhalin}:/";
          fsType = "nfs";
          options = [ "x-systemd.automount" "noauto" ];
        };
      };
    };

    # Home is in France/Paris, so set the timezone accordingly
    time.timeZone = "Europe/Paris";

    # Because we are at home, we can make assumption around the network
  };
}
