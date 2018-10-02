{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "knctl-${version}";
  version = "0.0.9";
  rev = "v${version}";

  goPackagePath = "github.com/cppforlife/knctl";
  subPackages = [ "cmd/knctl" ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "cppforlife";
    repo = "knctl";
    sha256 = "0vfn5z9w44qkjymsy6rpgk5yz36i4r54c3f1zrr628mfd0jk0mx7";
  };

  meta = {
    description = "Knative CLI";
    homepage = https://github.com/cppforlife/knctl;
    licence = lib.licenses.asl20;
  };
}
