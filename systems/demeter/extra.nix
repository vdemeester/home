{
  libx,
  lib,
  globals,
  ...
}:
{
  imports = [
    ../common/services/bind.nix
    ../common/services/prometheus-exporters-node.nix
    ../common/services/prometheus-exporters-bind.nix
  ];

  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;
  services = {
    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.demeter.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      # TODO: should probably switch to sync.sbr.pm or something maybe ?
      virtualHosts."demeter.sbr.pm" = {
        locations = lib.attrsets.mapAttrs' (
          name: value:
          lib.attrsets.nameValuePair "/syncthing/${name}/" {
            proxyPass = "http://${builtins.head value.net.vpn.ips}:8384/";
            recommendedProxySettings = true;
          }
        ) (lib.attrsets.filterAttrs (_name: value: (libx.hasVPNips value)) globals.machines);
        # // {
        #   "/n8n/" = {
        #     proxyPass = "http://127.0.0.1:5678/";
        #     recommendedProxySettings = true;
        #   };
        # };
      };
      virtualHosts."demeter.vpn" = {
        locations = lib.attrsets.mapAttrs' (
          name: value:
          lib.attrsets.nameValuePair "/syncthing/${name}/" {
            proxyPass = "http://${builtins.head value.net.vpn.ips}:8384/";
            recommendedProxySettings = true;
          }
        ) (lib.attrsets.filterAttrs (_name: value: (libx.hasVPNips value)) globals.machines);
      };
      virtualHosts."demeter.home" = {
        locations = lib.attrsets.mapAttrs' (
          name: value:
          lib.attrsets.nameValuePair "/syncthing/${name}/" {
            proxyPass = "http://${builtins.head value.net.ips}:8384/";
            recommendedProxySettings = true;
          }
        ) (lib.attrsets.filterAttrs (_name: value: (libx.hasIps value)) globals.machines);
      };
    };
  };

  # TODO: could be enable by default for all ?
  security.pam.enableSSHAgentAuth = true;
}
