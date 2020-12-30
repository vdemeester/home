# Profiles are grouping modules so that we don't have to
# specify them for all machines all the time.
{ lib, ... }:

{
  imports = [
    ./base.flake.nix
    ./home.flake.nix
    ./laptop.flake.nix
    # ./desktop.flake.nix
    # FIXME: vpn, server, builder, …
  ];

  modules.base.enable = lib.mkDefault true;
}
