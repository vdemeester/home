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
    };
  };
  config = mkIf cfg.enable {
    environment.systemPackages = [ pkgs.wireguard-tools ];
    boot.kernel.sysctl."net.ipv4.ip_forward" = lib.mkForce 1; # FIXME should probably be mkDefault

    # Enable nftables and configure NAT/forwarding rules for WireGuard
    networking.nftables = {
      enable = true;
      tables = {
        wireguard-nat = {
          family = "ip";
          content = ''
            chain postrouting {
              type nat hook postrouting priority 100; policy accept;
              ip saddr 10.100.0.0/24 masquerade
            }
          '';
        };
        wireguard-filter = {
          family = "inet";
          content = ''
            chain forward {
              type filter hook forward priority 0; policy accept;
              iifname "wg0" accept
              oifname "wg0" accept
            }
          '';
        };
      };
    };

    networking.firewall.allowedUDPPorts = [ 51820 ];
    networking.firewall.trustedInterfaces = [ "wg0" ];
    networking.wireguard.enable = true;
    networking.wireguard.interfaces = {
      "wg0" = {
        inherit (cfg) ips peers;
        listenPort = 51820;
        privateKeyFile = "/etc/wireguard/private.key";
      };
    };
  };
}
