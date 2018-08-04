{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "dobi-${version}";
  version = "v0.9";
  rev = "0cb9d4c4290d8c044b34f1fb5c662ac4116c2f25";

  buildFlagsArray = let t = "${goPackagePath}/cmd"; in ''
    -ldflags=
       -X ${t}.gitsha=${rev}
       -X ${t}.buildDate=unknown
  '';
  goPackagePath = "github.com/dnephin/dobi";
  excludedPackages = "docs";

  src = fetchFromGitHub {
    inherit rev;
    owner = "dnephin";
    repo = "dobi";
    sha256 = "11a3nhhxcixz3sb92vr57gj5dpg4h47xy2v0wczxzcn1fhf2s06n";
  };

  goDeps = ./deps.nix;

  meta = {
    description = "A build automation tool for Docker applications";
    homepage = https://dnephin.github.io/dobi/;
    license = lib.licenses.asl20;
  };
}
