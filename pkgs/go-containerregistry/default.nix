{ stdenv, lib, buildGoPackage, fetchgit }:

buildGoPackage rec {
  name = "go-containerregistry-unstable-${version}";
  version = "2018-09-19";
  rev = "52f3c54ec23c758ce5375754a5de62f7cd5ebbbc";

  goPackagePath = "github.com/google/go-containerregistry";

  src = fetchgit {
    inherit rev;
    url = "https://github.com/google/go-containerregistry";
    sha256 = "1aqx3kcwgga2x7sbwyb4026f5phvnaavla688sjv6bswdxb1nnwr";
  };

  goDeps = ./deps.nix;
  subPackages = [
    "cmd/crane" "cmd/gcrane" "cmd/ko"
  ];

  meta = {
    description = "Go library and CLIs for working with container registries";
    homepage = https://github.com/google/go-containerregistry;
    license = lib.licenses.asl20;
  };
}
