# This configuration file simply determines the hostname and then import both
# the default configuration (common for all machine) and specific machine
# configuration.
{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixos-unstable
, ...
}:
let
  hostName = "${builtins.readFile ./hostname}";
  sources = import ./nix/sources.nix;
in
{
  imports = with sources; [
    # Generated hardware configuration
    ./hardware-configuration.nix
    # Load home-manager nixos module
    "${home-manager}/nixos"
    # Default profile with default configuration
    ./modules/module-list.nixos.nix
    # Set the machine to nixos
    ./machines/is-nixos.nix
    # Machine specific configuration files
    (./machines + "/${hostName}.nixos.nix")
  ];

  networking.hostName = "${hostName}";
}
