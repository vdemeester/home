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
  cfg = config.services.wireguard;
in
{
  options = {
    services.wireguard = {
      enable = mkEnableOption "Enable a wireguard client";
      ips = mkOption {
        type = with types; listOf str;
        description = ''
          The peer IPs
        '';
      };
      allowedIPs = mkOption {
        default = [ "10.100.0.0/24" ];
        type = with types; listOf str;
        description = ''
          The peer (server) allowedIPs
        '';
      };
      endpoint = mkOption {
        type = with types; str;
        description = ''
          The endpoint IP to target
        '';
      };
      endpointPort = mkOption {
        default = 51820;
        type = with types; int;
        description = ''
          The endpoint Port to target
        '';
      };
      endpointPublicKey = mkOption {
        type = with types; str;
        description = ''
          The peer (server) public key
        '';
      };
      mtu = mkOption {
        type = with types; nullOr int;
        default = null;
        description = ''
          MTU size for the WireGuard interface.
          Common values: 1420 (conservative), 1380 (for PPPoE).
          If null, uses system default.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    assertions = [
      {
        assertion = cfg.endpoint != "";
        message = "services.wireguard.endpoint must be set.";
      }
      {
        assertion = cfg.endpointPublicKey != "";
        message = "services.wireguard.endpointPublicKey must be set.";
      }
      {
        assertion = cfg.ips != [ ];
        message = "services.wireguard.ips must be set.";
      }
    ];
    environment.systemPackages = [ pkgs.wireguard-tools ];
    networking.firewall.trustedInterfaces = [ "wg0" ];
    networking.wireguard.enable = true;
    networking.wireguard.interfaces = {
      wg0 = {
        inherit (cfg) ips;
        privateKeyFile = "/etc/wireguard/private.key";
        peers = [
          {
            publicKey = cfg.endpointPublicKey;
            inherit (cfg) allowedIPs;
            endpoint = "${cfg.endpoint}:${toString cfg.endpointPort}";
            persistentKeepalive = 25;
          }
        ];
      }
      // lib.optionalAttrs (cfg.mtu != null) { inherit (cfg) mtu; };
    };
  };
}
