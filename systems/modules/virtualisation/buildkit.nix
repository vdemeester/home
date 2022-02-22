{ config, lib, pkgs, ... }:
let
  cfg = config.virtualisation.buildkitd;
  inherit (lib) mkOption mkIf types;
in
{
  options.virtualisation.buildkitd = {
    enable = mkOption {
      type = types.bool;
      default = false;
      description =
        ''
          This option enables buildkitd
        '';
    };

    package = mkOption {
      default = pkgs.buildkit;
      type = types.package;
      example = pkgs.buildkit;
      description = ''
        Buildkitd package to be used in the module
      '';
    };

    packages = mkOption {
      type = types.listOf types.package;
      default = [ pkgs.runc pkgs.git ];
      description = "List of packages to be added to buildkitd service path";
    };

    extraOptions = mkOption {
      type = types.separatedString " ";
      default = "";
      description =
        ''
          The extra command-line options to pass to
          <command>buildkitd</command> daemon.
        '';
    };
  };

  config = mkIf cfg.enable {
    users.groups.buildkit.gid = 350;
    environment.systemPackages = [ cfg.package ];
    systemd.packages = [ cfg.package ];
    systemd.services.buildkitd = {
      after = [ "network.target" "containerd.service" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = ''${cfg.package}/bin/buildkitd --addr=unix:///run/buildkit/buildkitd.sock --group=buildkit ${cfg.extraOptions}'';
        Delegate = "yes";
        KillMode = "process";
        Type = "notify";
        Restart = "always";
        RestartSec = "10";

        # "limits" defined below are adopted from upstream: https://github.com/containerd/containerd/blob/master/containerd.service
        LimitNPROC = "infinity";
        LimitCORE = "infinity";
        LimitNOFILE = "infinity";
        TasksMax = "infinity";
        OOMScoreAdjust = "-999";
      };
      path = [ cfg.package ] ++ cfg.packages;
    };

  };


}
