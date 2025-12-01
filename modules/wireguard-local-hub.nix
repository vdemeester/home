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
  cfg = config.services.wireguard.localHub;
in
{
  options = {
    services.wireguard.localHub = {
      enable = mkEnableOption "Enable a local wireguard hub";
      ips = mkOption {
        type = with types; listOf str;
        description = ''
          The hub IPs on the VPN network
        '';
      };
      listenAddress = mkOption {
        type = with types; str;
        description = ''
          The local network IP address to listen on
        '';
      };
      listenPort = mkOption {
        default = 51821;
        type = with types; int;
        description = ''
          The port to listen on
        '';
      };
      peers = mkOption {
        default = [ ];
        description = "Peers linked to the local hub interface.";
        type = with types; listOf anything;
      };
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wireguard-tools ];
    boot.kernel.sysctl."net.ipv4.ip_forward" = lib.mkForce 1;
    networking.firewall.extraCommands = ''
      iptables -t nat -A POSTROUTING -s 10.100.0.0/24 -o wg-local -j MASQUERADE
      iptables -A FORWARD -i wg-local -j ACCEPT
    '';
    # Allow local hub port on LAN interface
    networking.firewall.allowedUDPPorts = [ cfg.listenPort ];
    networking.firewall.trustedInterfaces = [ "wg-local" ];
    networking.wireguard.enable = true;
    networking.wireguard.interfaces = {
      "wg-local" = {
        inherit (cfg) ips peers;
        listenPort = cfg.listenPort;
        # Listen on specific local IP
        socketNamespace = null;
        privateKeyFile = "/etc/wireguard/private.key";
        postSetup = ''
          # Ensure we're listening on the correct interface
          ${pkgs.iproute2}/bin/ip route add 10.100.0.0/24 dev wg-local metric 100 || true
        '';
      };
    };
  };
}
