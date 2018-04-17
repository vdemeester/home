{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "clasp-${version}";
  version = "0.0.1";
  rev = "v${version}";

  goPackagePath = "github.com/vdemeester/clasp";

  src = fetchFromGitHub {
    inherit rev;
    owner = "vdemeester";
    repo = "clasp";
    sha256 = "0vvb3ay635svp710z8kn499426ajf5x83i4fih6zwp32fnmwvfwn";
  };

  meta = {
    description = "mini hook / rebuild configuration binary written in Go";
    homepage = "https://github.com/vdemeester/clasp";
    licence = lib.licenses.asl20;
  };
}
