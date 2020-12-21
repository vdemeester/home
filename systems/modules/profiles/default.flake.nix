{ lib, ... }:

{
  imports = [
    ./base.flake.nix
    ./desktop.flake.nix
    ./development.flake.nix
    ./home.flake.nix
    ./i3.nix
    ./laptop.flake.nix
    # FIXME: vpn, server, builder, …
  ];

  profiles.base.enable = lib.mkDefault true;
}
