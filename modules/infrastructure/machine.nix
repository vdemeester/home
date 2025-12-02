{ config, lib, ... }:
let
  cfg = config.infrastructure.machine;
in
{
  options.infrastructure.machine = {
    enable = lib.mkEnableOption "machine infrastructure configuration";

    hostname = lib.mkOption {
      type = lib.types.str;
      description = "Machine hostname";
      default = config.networking.hostName;
    };

    network = {
      localIPs = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "Local network IP addresses for this machine";
      };

      dnsNames = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
        description = "DNS names for this machine";
      };

      vpn = {
        enable = lib.mkOption {
          type = lib.types.bool;
          default = false;
          description = "Whether this machine is on the VPN";
        };

        publicKey = lib.mkOption {
          type = lib.types.nullOr lib.types.str;
          default = null;
          description = "WireGuard public key for this machine";
        };

        ips = lib.mkOption {
          type = lib.types.listOf lib.types.str;
          default = [ ];
          description = "VPN IP addresses for this machine";
        };
      };
    };

    ssh = {
      hostKey = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "SSH host key for this machine";
      };
    };

    syncthing = {
      enable = lib.mkOption {
        type = lib.types.bool;
        default = false;
        description = "Whether Syncthing is configured on this machine";
      };

      deviceID = lib.mkOption {
        type = lib.types.nullOr lib.types.str;
        default = null;
        description = "Syncthing device ID";
      };

      folders = lib.mkOption {
        type = lib.types.attrsOf (
          lib.types.submodule {
            options = {
              type = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = "Folder type (e.g., 'receiveonly')";
              };

              path = lib.mkOption {
                type = lib.types.nullOr lib.types.str;
                default = null;
                description = "Custom path for this folder";
              };

              paused = lib.mkOption {
                type = lib.types.bool;
                default = false;
                description = "Whether this folder should start paused";
              };
            };
          }
        );
        default = { };
        description = "Syncthing folder configurations for this machine";
      };
    };
  };

  config = lib.mkIf cfg.enable {
    # This module just defines the options, configuration is done per-host
  };
}
