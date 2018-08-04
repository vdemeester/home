{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "tuck-${version}";
  version = "0.1.1";
  rev = "v${version}";

  goPackagePath = "github.com/vdemeester/tuck";

  src = fetchFromGitHub {
    inherit rev;
    owner = "vdemeester";
    repo = "tuck";
    sha256 = "1wwwh6wq5z0xdix2pdxh6i72f5w8qyc8rsc29b50vl00z869igv8";
  };

  meta = {
    description = "symlink farm manager à-la-stow";
    homepage = "https://github.com/vdemeester/tuck";
    licence = lib.licenses.asl20;
  };
}
