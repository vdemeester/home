{ stdenv, lib, buildGoPackage, fetchgit }:

buildGoPackage rec {
  name = "prm-unstable-${version}";
  version = "2018-04-30";
  rev = "708f8af64ef7faca358406496f81f83d33f28c12";

  goPackagePath = "github.com/ldez/prm";

  src = fetchgit {
    inherit rev;
    url = "https://github.com/ldez/prm";
    sha256 = "16314icypsnw1b8vd306sd3752cz80ziq98mcayxh2bh4wk59hr7";
  };

  goDeps = ./deps.nix;

  # TODO: add metadata https://nixos.org/nixpkgs/manual/#sec-standard-meta-attributes
  meta = {
    description = "Pull Request Manager for Maintainers";
    homepage = "https://github.com/ldez/prm";
    licence = lib.licences.asl20;
  };
}
