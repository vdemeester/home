{ stdenv, lib, fetchFromGitHub, poetry2nix, python310, python310Packages }:

poetry2nix.mkPoetryApplication rec {
  pname = "sugarjazy";
  version = "0.5.1";
  python = python310;

  projectDir = ./.;
  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "sugarjazy";
    rev = "${version}";
    sha256 = "sha256-GkfPpCRXta2aDUQT4c/tU+T1b/LoPM+k1GlopvgIpzQ=";
  };
  doCheck = false;

  buildInputs = [ python310Packages.dateutil ];

  meta = with lib; {
    homepage = "https://github.com/chmouel/sugarjazy";
    description = "parse json logs nicely";
    license = licenses.asl20;
  };
}
