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

    autostart = mkOption {
      type = types.bool;
      default = true;
      description = ''
        Start buildkitd automatically.
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
      wants = [ "containerd.service" ];
      after = [ "containerd.service" ];
      wantedBy = lib.optional cfg.autostart [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = [
          ""
          ''
            ${cfg.package}/bin/buildkitd \
              ${cfg.extraOptions}
          ''
        ];
      };
      path = [ cfg.package ] ++ cfg.packages;
    };


    systemd.sockets.buildkitd = {
      description = "Buildkitd Socket for the API";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = "/run/buildkitd/buildkitd.sock";
        SocketMode = "0660";
        SocketUser = "root";
        SocketGroup = "buildkit";
      };
    };

  };


}
