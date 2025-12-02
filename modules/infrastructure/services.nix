{ lib, ... }:
{
  options.infrastructure.services = lib.mkOption {
    type = lib.types.attrsOf (
      lib.types.submodule {
        options = {
          host = lib.mkOption {
            type = lib.types.str;
            description = "Hostname where this service runs";
          };

          aliases = lib.mkOption {
            type = lib.types.listOf lib.types.str;
            default = [ ];
            description = "Short aliases for this service";
          };
        };
      }
    );
    default = {
      # Media services on rhea
      immich = {
        host = "rhea";
      };
      jellyfin = {
        host = "rhea";
      };
      jellyseerr = {
        host = "rhea";
      };
      sonarr = {
        host = "rhea";
      };
      radarr = {
        host = "rhea";
      };
      lidarr = {
        host = "rhea";
      };
      bazarr = {
        host = "rhea";
      };
      transmission = {
        host = "rhea";
        aliases = [ "t" ];
      };
      syncthing = {
        host = "rhea";
        aliases = [ "s" ];
      };
      # MQTT on demeter (routed through rhea/traefik)
      mqtt = {
        host = "rhea";
      };
      # Services on sakhalin (routed through rhea/traefik)
      kiwix = {
        host = "rhea";
      };
      n8n = {
        host = "rhea";
      };
      paperless = {
        host = "rhea";
      };
      grafana = {
        host = "rhea";
      };
    };
    description = "Service to host mappings";
  };
}
