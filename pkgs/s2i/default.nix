{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "s2i-${version}";
  version = "1.1.11";
  rev = "v${version}";

  goPackagePath = "github.com/openshift/source-to-image";
  subPackages = [ "cmd/s2i" ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "openshift";
    repo = "source-to-image";
    sha256 = "0mi4wnvawlgsv4zxg6skg022lqyshgyr3xab5sb2rvqvy0wiqvmr";
  };

  meta = {
    description = "A tool for building/building artifacts from source and injecting into docker images";
    homepage = https://github.com/openshift/source-to-image;
    licence = lib.licenses.asl20;
  };
}
