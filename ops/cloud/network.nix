let
  sources = import ../../nix;
  pkgs = sources.pkgs { };
in
{
  network = {
    inherit pkgs;
    description = "Cloud network";
  };

  "kerkouane" = { config, pkgs, lib, ... }: {
    deployment.targetUser = "root";
    deployment.targetHost = "kerkouane.vpn";
    imports = [ ../../systems/hosts/kerkouane.nix ];
  };
}
