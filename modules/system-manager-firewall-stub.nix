# Stub module for networking.firewall on system-manager
# Some nixpkgs modules try to configure firewall even when disabled
# This provides the minimal interface they expect
{ lib, ... }:
{
  options = {
    networking.firewall = {
      enable = lib.mkEnableOption "firewall" // {
        default = false;
      };
      allowedTCPPorts = lib.mkOption {
        type = lib.types.listOf lib.types.port;
        default = [ ];
      };
      allowedUDPPorts = lib.mkOption {
        type = lib.types.listOf lib.types.port;
        default = [ ];
      };
      trustedInterfaces = lib.mkOption {
        type = lib.types.listOf lib.types.str;
        default = [ ];
      };
    };
  };

  # No config needed - this is just a stub to prevent errors
}
