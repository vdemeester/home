{
  config,
  lib,
  ...
}:
{
  options = {
    networking.bridge-common = {
      enable = lib.mkEnableOption "Common network bridge configuration";
      interface = lib.mkOption {
        type = lib.types.str;
        default = "enp0s31f6";
        description = "Network interface to bridge";
      };
    };
  };

  config = lib.mkIf config.networking.bridge-common.enable {
    networking = {
      bridges.br1.interfaces = [ config.networking.bridge-common.interface ];
      useDHCP = false;
      interfaces.br1 = {
        useDHCP = true;
      };
      firewall.enable = false; # we are in safe territory :D
    };
  };
}