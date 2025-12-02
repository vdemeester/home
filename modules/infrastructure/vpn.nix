{ lib, ... }:
{
  options.infrastructure.vpn = {
    endpoint = lib.mkOption {
      type = lib.types.str;
      default = "167.99.17.238";
      description = "VPN server endpoint IP address";
    };
  };
}
