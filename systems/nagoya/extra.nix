{
  globals,
  lib,
  pkgs,
  ...
}:
{
  networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  services = {
    wireguard = {
      enable = true;
      ips = globals.fn.wg-ips globals.machines.nagoya.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
  };

  services.nginx = {
    enable = true;
    recommendedGzipSettings = true;
    recommendedTlsSettings = true;
    recommendedOptimisation = true;
    virtualHosts."nagoya.sbr.pm" = {
      locations = lib.attrsets.mapAttrs' (
        name: value:
        lib.attrsets.nameValuePair "/syncthing/${name}/" {
          proxyPass = "http://${builtins.head value.net.vpn.ips}:8384/";
          recommendedProxySettings = true;
        }
      ) (lib.attrsets.filterAttrs (_name: value: (globals.fn.hasVPNips value)) globals.machines);
    };
    virtualHosts."nagoya.vpn" = {
      locations = lib.attrsets.mapAttrs' (
        name: value:
        lib.attrsets.nameValuePair "/syncthing/${name}/" {
          proxyPass = "http://${builtins.head value.net.vpn.ips}:8384/";
          recommendedProxySettings = true;
        }
      ) (lib.attrsets.filterAttrs (_name: value: (globals.fn.hasVPNips value)) globals.machines);
    };
  };

  environment.systemPackages = with pkgs; [
    lm_sensors
  ];
}
