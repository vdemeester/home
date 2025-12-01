{
  config,
  lib,
  pkgs,
  ...
}:
let
  inherit (lib)
    mkEnableOption
    mkIf
    mkOption
    types
    ;
  cfg = config.services.wireguard.dualClient;
in
{
  options = {
    services.wireguard.dualClient = {
      enable = mkEnableOption "Enable a dual-hub wireguard client";
      ips = mkOption {
        type = with types; listOf str;
        description = ''
          The client IPs on the VPN network
        '';
      };
      allowedIPs = mkOption {
        default = [ "10.100.0.0/24" ];
        type = with types; listOf str;
        description = ''
          The allowed IPs for VPN traffic
        '';
      };

      # Local hub configuration
      localHub = {
        enable = mkEnableOption "Enable local hub connection" // {
          default = true;
        };
        endpoint = mkOption {
          type = with types; str;
          description = ''
            The local hub IP address
          '';
        };
        endpointPort = mkOption {
          default = 51821;
          type = with types; int;
          description = ''
            The local hub port
          '';
        };
        endpointPublicKey = mkOption {
          type = with types; str;
          description = ''
            The local hub public key
          '';
        };
      };

      # Remote hub configuration
      remoteHub = {
        endpoint = mkOption {
          type = with types; str;
          description = ''
            The remote hub IP address
          '';
        };
        endpointPort = mkOption {
          default = 51820;
          type = with types; int;
          description = ''
            The remote hub port
          '';
        };
        endpointPublicKey = mkOption {
          type = with types; str;
          description = ''
            The remote hub public key
          '';
        };
      };
    };
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.remoteHub.endpoint != "";
        message = "services.wireguard.dualClient.remoteHub.endpoint must be set.";
      }
      {
        assertion = cfg.remoteHub.endpointPublicKey != "";
        message = "services.wireguard.dualClient.remoteHub.endpointPublicKey must be set.";
      }
      {
        assertion = cfg.ips != [ ];
        message = "services.wireguard.dualClient.ips must be set.";
      }
    ];

    environment.systemPackages = [ pkgs.wireguard-tools ];
    networking.firewall.trustedInterfaces = [
      "wg0"
      "wg-local"
    ];
    networking.wireguard.enable = true;

    # Remote hub interface (wg0) - fallback
    networking.wireguard.interfaces.wg0 = {
      inherit (cfg) ips;
      privateKeyFile = "/etc/wireguard/private.key";
      peers = [
        {
          publicKey = cfg.remoteHub.endpointPublicKey;
          inherit (cfg) allowedIPs;
          endpoint = "${cfg.remoteHub.endpoint}:${toString cfg.remoteHub.endpointPort}";
          persistentKeepalive = 25;
        }
      ];
      postSetup = ''
        # Add route with higher metric (fallback)
        ${pkgs.iproute2}/bin/ip route add 10.100.0.0/24 dev wg0 metric 200 || true
      '';
      postShutdown = ''
        ${pkgs.iproute2}/bin/ip route del 10.100.0.0/24 dev wg0 metric 200 || true
      '';
    };

    # Local hub interface (wg-local) - preferred
    networking.wireguard.interfaces.wg-local = mkIf cfg.localHub.enable {
      inherit (cfg) ips;
      privateKeyFile = "/etc/wireguard/private.key";
      peers = [
        {
          publicKey = cfg.localHub.endpointPublicKey;
          inherit (cfg) allowedIPs;
          endpoint = "${cfg.localHub.endpoint}:${toString cfg.localHub.endpointPort}";
          persistentKeepalive = 25;
        }
      ];
      postSetup = ''
        # Add route with lower metric (preferred)
        ${pkgs.iproute2}/bin/ip route add 10.100.0.0/24 dev wg-local metric 100 || true
      '';
      postShutdown = ''
        ${pkgs.iproute2}/bin/ip route del 10.100.0.0/24 dev wg-local metric 100 || true
      '';
    };
  };
}
