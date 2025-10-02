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
    networking.firewall.extraCommands = ''
      iptables -t nat -A POSTROUTING -s10.100.0.0/32 -j MASQUERADE
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
      };
    };
  };
}
