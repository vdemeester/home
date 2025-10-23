{
  config,
  globals,
  lib,
  libx,
  pkgs,
  ...
}:
{
  imports = [
    # ../common/services/containers.nix
    ../common/services/docker.nix
  ];
  system.nixos.tags =
    let
      cfg = config.boot.loader.raspberryPi;
    in
    [
      "raspberry-pi-${cfg.variant}"
      cfg.bootloader
      config.boot.kernelPackages.kernel.version
    ];
  # networking.firewall.enable = false;

  # TODO make it an option ? (otherwise I'll add it for all)
  users.users.vincent.linger = true;

  services = {
    firefly-iii = {
      enable = true;
      virtualHost = "ffiii.nagoya.sbr.pm";
      enableNginx = true;
      settings.APP_KEY_FILE = "/etc/ffiii.keyfile";
    };
    firefly-iii-data-importer = {
      enable = true;
      enableNginx = true;
      virtualHost = "import.ffiii.nagoya.sbr.pm";
    };
    # paperless = {
    #   enable = true;
    #   domain = "paperless.nagoya.sbr.pm";
    #   configureNginx = true;
    # };

    wireguard = {
      enable = true;
      ips = libx.wg-ips globals.machines.nagoya.net.vpn.ips;
      endpoint = "${globals.net.vpn.endpoint}";
      endpointPublicKey = "${globals.machines.kerkouane.net.vpn.pubkey}";
    };
    nginx = {
      enable = true;
      recommendedGzipSettings = true;
      recommendedTlsSettings = true;
      recommendedOptimisation = true;
      # TODO: should probably switch to sync.sbr.pm or something maybe ?
      virtualHosts."nagoya.sbr.pm" = {
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
      virtualHosts."nagoya.vpn" = {
        locations = lib.attrsets.mapAttrs' (
          name: value:
          lib.attrsets.nameValuePair "/syncthing/${name}/" {
            proxyPass = "http://${builtins.head value.net.vpn.ips}:8384/";
            recommendedProxySettings = true;
          }
        ) (lib.attrsets.filterAttrs (_name: value: (libx.hasVPNips value)) globals.machines);
      };
      virtualHosts."nagoya.home" = {
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

  environment.systemPackages = with pkgs; [
    lm_sensors
  ];
}
