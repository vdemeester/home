{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "krew-unstable-${version}";
  version = "2018-09-26";
  rev = "4f7485c1bdd303efee97ab709488829b222e5335";

  goPackagePath = "github.com/GoogleContainerTools/krew";

  src = fetchFromGitHub {
    inherit rev;
    owner = "GoogleContainerTools";
    repo = "krew";
    sha256 = "0rv7gikvnwj0lrn8f1v868jfs4i8hkqv5j289lgr5hzr0pjx9v8a";
  };

  meta = {
    description = "The package manager for 'kubectl plugins. ";
    homepage = "https://github.com/GoogleContainerTools/krew";
    licence = lib.licenses.asl20;
  };
}
