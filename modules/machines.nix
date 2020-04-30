{ config, lib, ... }:

with lib; {
  options = {
    machine = {
      home-manager = mkEnableOption "It is a home-manager configuration";
      nixos = mkEnableOption "It is a nixos configuration";
    };
  };
}
