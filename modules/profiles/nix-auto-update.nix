{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.nix-auto-update;
in
{
  options = {
    profiles.nix-auto-update = {
      enable = mkOption {
        default = true;
        description = "Enable nix-auto-update profile";
        type = types.bool;
      };
      autoUpgrade = mkOption {
        default = true;
        description = "Automatically try to upgrade the system";
        type = types.bool;
      };
      dates = mkOption {
        default = "weekly";
        description = "Specification (in the format described by systemd.time(7)) of the time at which the auto-update will run. ";
        type = types.str;
      };
      version = mkOption {
        default = "18.09";
        description = "System version (NixOS)";
        type = types.str;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      system = {
        stateVersion = cfg.version;
      };
      # Auto refresh nix-channel each day
      systemd.services.channel-update = {
        description = "Update nix-channel daily";
        wantedBy = [ "multi-user.target" ];
        serviceConfig = {
          Type = "oneshot";
          ExecStart = "/run/current-system/sw/bin/nix-channel --update";
          Environment = "PATH=/run/current-system/sw/bin";
        };
      };
      systemd.timers.channel-update = {
        description = "Update nix-channel daily";
        wantedBy = [ "timers.target" ];
        timerConfig = {
          OnCalendar = "daily";
          Persistent = "true";
        };
      };
      systemd.timers.channel-update.enable = true;
    }
    (mkIf cfg.autoUpgrade {
      systemd.services.nixos-update = {
        description = "NixOS Upgrade";
        unitConfig.X-StopOnRemoval = false;
        restartIfChanged = false;
        serviceConfig.Type = "oneshot";
        environment = config.nix.envVars //
        { inherit (config.environment.sessionVariables) NIX_PATH;
          HOME = "/root";
        };
        path = [ pkgs.gnutar pkgs.xz pkgs.git config.nix.package.out ];
        script = ''
          cd /etc/nixos/
          git pull --autostash --rebase
          nix-channel --update
          /run/current-system/sw/bin/nixos-rebuild switch
        '';
        startAt = cfg.dates;
        onFailure = ["status-email-root@%n.service"];
      };
    })
  ]);
}
