{ sources ? import ./nix
, lib ? sources.lib
, pkgs ? sources.pkgs { }
, pkgs-unstable ? sources.pkgs-unstable { }
, pkgs-darwin ? sources.nix-darwin
, nixpkgs ? sources.nixpkgs { }
}:
with builtins; with lib;
let
  /*
  mkNixOS: make a nixos system build with the given name and cfg.

  cfg is an attributeSet:
  - arch is architecture (darwin is a special one)
  - channel is the "channel" to use (nixos stable, nixos unstable, …)
  - vm is whether it is a vm, or not

  Example:
    hokkaido = { arch = "x86_64-linux"; };
    naruhodo = { arch = "x86_64-linux"; channel = "unstable"; };
  */
  mkNixOS = name: cfg:
    let
      configuration = ./systems + "/${name}.nix";
      system = cfg.arch;
      # If channel == unstable, use nixos-unstable (pkgs-unstable) otherwise use nixos (pkgs)
      p =
        if cfg ? channel && cfg.channel == "unstable"
        then pkgs-unstable
        else pkgs;
      # If vm == true, build a VM, otherwise build the system
      nixos = import (p.path + "/nixos") { inherit configuration system; };
      main =
        if cfg ? vm && cfg.vm
        then nixos.vm
        else
          if system == "x86_64-darwin"
          then (import (pkgs.darwin.path) { inherit nixpkgs configuration; }).system
          else nixos.config.system.build;
    in
    main;
  /*
  mkSystem: make a system build with the given name and cfg.
  */
  mkSystem = name: cfg:
    if cfg ? vm && cfg.vm
    then (mkNixOS name cfg)
    else (mkNixOS name cfg).toplevel;
  # mkDigitalOceanImage = name: arch: (mkNixOS name arch).digitalocean

  systemAttrs = (mapAttrs mkSystem (import ./hosts.nix));

  filterSystems = arch: attrValues (filterAttrs (_:v: v.system == arch) systemAttrs);
  x86_64Systems = filterSystems "x86_64-linux";
  aarch64Systems = filterSystems "aarch64-linux";
  allSystems = attrValues systemAttrs;
in
{
  systems = nixpkgs.linkFarmFromDrvs "systems" allSystems;
  aarch64 = nixpkgs.linkFarmFromDrvs "aarch64" aarch64Systems;
  x86_64-linux = nixpkgs.linkFarmFromDrvs "x86_64-linux" x86_64Systems;
} // systemAttrs
