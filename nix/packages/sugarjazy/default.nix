{ stdenv, lib, fetchFromGitHub, poetry2nix, python310, python310Packages }:

poetry2nix.mkPoetryApplication rec {
  pname = "sugarjazy";
  version = "0.6.2";
  python = python310;

  projectDir = ./.;
  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "sugarjazy";
    rev = "${version}";
    sha256 = "sha256-UbYdyfLEsO4ghIiN1BzOdPqEujbRbKqrN2WCRlIj3g4=";
  };
  doCheck = false;

  propagatedBuildInputs = [ python310Packages.python-dateutil ];

  meta = with lib; {
    homepage = "https://github.com/chmouel/sugarjazy";
    description = "parse json logs nicely";
    license = licenses.asl20;
  };
}
