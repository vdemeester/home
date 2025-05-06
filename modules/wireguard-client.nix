{
  config,
  options,
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
      enable = mkEnableOption "Enable ";
      ips = mkOption {
        type = with types; listOf str;
        description = ''
          The client IPs
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
      };
    };
  };
}
