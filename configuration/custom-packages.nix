{ config, lib, pkgs, ... }:

{
  nixpkgs.config = {
  packageOverrides = self: with self; let
    fetchNixPkgs = { rev, sha256 }:
    fetchFromGitHub {
      inherit sha256 rev;
      owner = "vdemeester";
      repo = "sbrpkgs";
    };
    pinnedPkgs = import (fetchNixPkgs {
      rev = "8e0a7940b2e18996dd18905b444b65f08d5917fa";
      sha256 = "05wg6sa7hi95zppkr3lj8sgxfisz7m8293wbgav3glvmv80jhwif";
    }) {};

  in {
    inherit (pinnedPkgs) doctl dobi vndr sbr-i3-config;
  };
  };
}
