{ lib, inputs, ... }:

{
  imports = [
    ./base.nix
    ./desktop.nix
    ./development.nix
    ./home.nix
    ./laptop.nix
    # FIXME: vpn, server, builder, …
  ];

  profiles.base.enable = lib.mkDefault true;
}
