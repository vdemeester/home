{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "dobi-${version}";
  version = "v0.11.1";
  rev = "7cca562fb4df4d06d0b0107108478d3d434c775f";

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
    sha256 = "0pv622dgdpl2nd4yvhjx6pgr1cd2bz5v9pisiaxivh1n7x5y3nzq";
  };

  goDeps = ./deps.nix;

  meta = {
    description = "A build automation tool for Docker applications";
    homepage = https://dnephin.github.io/dobi/;
    license = lib.licenses.asl20;
  };
}
