{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.services.athens;
in
{
  options = {
    services.athens = {
      enable = mkEnableOption ''
      Athens is a go module proxy
      '';
      package = mkOption {
        type = types.package;
        default = pkgs.nur.repos.vdemeester.athens;
        description = ''
          Athens package to use.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    systemd.packages = [ cfg.package ];
    environment.etc."athens/config.toml".text = ''
      GoBinary = "${pkgs.go}/bin/go"
      # what is that ?
      GoEnv = "development"
      GoGetWorkers = 30
      ProtocolWorkers = 30
      LogLevel = "debug"
      BuffaloLogLevel = "debug"
      Port = ":3000"
      ForceSSL = false
      StoragType = "disk"

      [Storage]
        [Storage.Disk]
          RootPath = "/var/lib/athens"
    '';
    systemd.services.athens = {
      description = "Athens service";
      after = [ "network.target" ];
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        Restart = "on-failure";
        ExecStart = ''
          ${cfg.package}/bin/proxy -config_file=/etc/athens/config.toml
        '';
        path = [ cfg.package ] ++ [ pkgs.go ];
      };
    };
  };
}
