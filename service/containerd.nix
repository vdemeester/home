# Systemd services for containerd.
{ config, lib, pkgs, ... }:

with lib;

let

  cfg = config.virtualisation.containerd;
#   proxy_env = optionalAttrs (pro != null) { Environment = "\"http_proxy=${pro}\""; };

in

{
  ###### interface

  options.virtualisation.containerd = {
    enable =
      mkOption {
      type = types.bool;
      default = false;
      description =
      ''
        This option enables containerd, a daemon that manages
        linux containers.
      '';
    };

    packages = mkOption {
      type = types.listOf types.package;
      default = [];
      description = "List of packages to be added to containerd service path";
    };

    listenOptions =
      mkOption {
      type = types.listOf types.str;
      default = ["/run/containerd/containerd.sock"];
      description =
      ''
        A list of unix and tcp containerd should listen to. The format follows
        ListenStream as described in systemd.socket(5).
      '';
    };

    extraOptions =
      mkOption {
      type = types.separatedString " ";
      default = "";
      description =
      ''
        The extra command-line options to pass to
        <command>containerd</command> daemon.
      '';
    };
  };

  ###### implementation

  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.containerd ];
    #   users.extraGroups.docker.gid = config.ids.gids.docker;
    systemd.packages = [ pkgs.containerd ];

    systemd.services.containerd = {
      wantedBy = [ "multi-user.target" ];
      serviceConfig = {
        ExecStart = [
          ""
        ''
          ${pkgs.containerd}/bin/containerd \
          ${cfg.extraOptions}
        ''];
        /*
        ExecReload=[
        ""
        "${pkgs.procps}/bin/kill -s HUP $MAINPID"
        ];
        */
        };
      path = [ pkgs.containerd ] ++ cfg.packages;
    };

    systemd.sockets.containerd = {
      description = "Containerd Socket for the API";
      wantedBy = [ "sockets.target" ];
      socketConfig = {
        ListenStream = cfg.listenOptions;
        SocketMode = "0660";
        SocketUser = "root";
        SocketGroup = "root";
      };
    };
  };
}
