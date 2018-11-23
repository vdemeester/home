# This configuration file simply determines the hostname and then import both
# the default configuration (common for all machine) and specific machine
# configuration.

{ config, pkgs, ... }:

let
  hostName = "${builtins.readFile ./hostname}";
in
rec {
  imports = [
    # Generated hardware configuration
    ./hardware-configuration.nix
    # Default profile with default configuration
    ./modules/module-list.nix
    # Machine specific configuration files
    (./machine + "/${hostName}.nix")
  ];
  
  networking.hostName = "${hostName}";
}


