# This configuration file simply determines the hostname and then import both
# the default configuration (common for all machine) and specific machine
# configuration.
{ sources ? import ../nix/sources.nix
, pkgs ? import sources.nixpkgs { }
, ...
}:
let
  hostName = "${builtins.readFile ./hostname}";
  sources = import ./nix/sources.nix;
in
{
  programs = {
    home-manager = {
      enable = true;
      path = "${sources.home-manager}";
    };
  };
  imports = with sources; [
    # Default profile with default configuration
    ./modules/module-list.nix
    # Set the machine to home
    ./machines/is-hm.nix
    # Machine specific configuration files
    (./machines + "/${hostName}.nix")
  ];
}
