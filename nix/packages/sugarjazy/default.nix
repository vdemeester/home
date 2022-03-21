{ stdenv, lib, fetchFromGitHub, poetry2nix, python310 }:

poetry2nix.mkPoetryApplication rec {
  pname = "sugarjazy";
  version = "0.3.0";
  python = python310;

  projectDir = ./.;
  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "sugarjazy";
    rev = "${version}";
    sha256 = "sha256-stGHznSTsEOmd/tGnO+Tg/FixyKvRkOpv54GTyvmQJg=";
  };
  doCheck = false;

  meta = with lib; {
    homepage = "https://github.com/chmouel/sugarjazy";
    description = "parse json logs nicely";
    license = licenses.asl20;
  };
}
