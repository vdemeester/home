{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "prm-unstable-${version}";
  version = "2.1.1";
  rev = "83ff93d6d9d043d8c3dde54d47e8a82524bb8cf3"; # v2.1.1

  goPackagePath = "github.com/ldez/prm";

  buildFlagsArray = let t = "${goPackagePath}/meta"; in ''
    -ldflags=
       -X ${t}.Version=${version}
       -X ${t}.BuildDate=unknown
  '';

  src = fetchFromGitHub {
    inherit rev;
    owner = "ldez";
    repo = "prm";
    sha256 = "0adz9vli1x5f5v2vyfag7m2a9llj7bxih5g3ccjpiz22xk26rc6l";
  };

  goDeps = ./deps.nix;

  # TODO: add metadata https://nixos.org/nixpkgs/manual/#sec-standard-meta-attributes
  meta = {
    description = "Pull Request Manager for Maintainers";
    homepage = "https://github.com/ldez/prm";
    licence = lib.licences.asl20;
  };
}
