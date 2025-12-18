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
  cfg = config.services.wireguard.server;
in
{
  options = {
    services.wireguard.server = {
      enable = mkEnableOption "Enable a wireguard server";
      ips = mkOption {
        type = with types; listOf str;
        description = ''
          The peer IPs
        '';
      };
      peers = mkOption {
        default = [ ];
        description = "Peers linked to the interface.";
        type = with types; listOf anything;
      };
      mtu = mkOption {
        type = with types; nullOr int;
        default = 1420;
        description = ''
          MTU size for the WireGuard interface.
          Common values: 1420 (conservative), 1380 (for PPPoE).
          If null, uses system default.
        '';
      };
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wireguard-tools ];
    boot.kernel.sysctl."net.ipv4.ip_forward" = lib.mkForce 1; # FIXME should probably be mkDefault
    networking.firewall.extraCommands = ''
      iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -j MASQUERADE
      iptables -A FORWARD -i wg+ -j ACCEPT
    '';
    networking.firewall.allowedUDPPorts = [ 51820 ];
    networking.firewall.trustedInterfaces = [ "wg0" ];
    networking.wireguard.enable = true;
    networking.wireguard.interfaces = {
      "wg0" = {
        inherit (cfg) ips peers;
        listenPort = 51820;
        privateKeyFile = "/etc/wireguard/private.key";
      }
      // lib.optionalAttrs (cfg.mtu != null) { inherit (cfg) mtu; };
    };
  };
}
