{ lib, ... }:

{
  imports = [
    ./base.nix
    ./desktop.nix
    ./development.nix
    # FIXME: vpn, server, builder, …
  ];

  config.profiles.base.enable = lib.mkDefault true;
}
