{ lib, ... }:

{
  imports = [
    ./base.nix
    ./desktop.nix
    ./development.nix
    # FIXME: vpn, server, builder, …
  ];

  profiles.base.enable = lib.mkDefault true;
}
