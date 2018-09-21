{ stdenv, lib, fetchFromGitHub, buildGoPackage }:

buildGoPackage rec {
  name = "buildkit-unstable-${version}";
  version = "2018-09-20";
  rev = "39404586a50d1b9d0fb1c578cf0f4de7bdb7afe5";

goPackagePath = "github.com/moby/buildkit";

  src = fetchFromGitHub {
    inherit rev;
    owner = "moby";
    repo = "buildkit";
    sha256 = "05dcrsx3ysw35ar1qgzkij87y450fnf1j11rcrxpsndhd4sc06h8";
  };
}
