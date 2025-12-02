{ lib, ... }:
{
  options.infrastructure.dns = {
    cacheNetworks = lib.mkOption {
      type = lib.types.listOf lib.types.str;
      default = [
        "192.168.1.0/24"
        "10.100.0.0/24"
      ];
      description = "Networks allowed to use DNS cache";
    };

    zones = lib.mkOption {
      type = lib.types.listOf (
        lib.types.submodule {
          options = {
            name = lib.mkOption {
              type = lib.types.str;
              description = "Zone name";
            };

            master = lib.mkOption {
              type = lib.types.bool;
              default = true;
              description = "Whether this is a master zone";
            };

            slaves = lib.mkOption {
              type = lib.types.listOf lib.types.str;
              default = [ ];
              description = "Slave DNS servers for this zone";
            };

            file = lib.mkOption {
              type = lib.types.path;
              description = "Path to zone file";
            };
          };
        }
      );
      default = [ ];
      description = "DNS zone configurations";
    };
  };
}
