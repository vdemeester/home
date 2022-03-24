{ stdenv, lib, fetchFromGitHub, poetry2nix, python310, python310Packages }:

poetry2nix.mkPoetryApplication rec {
  pname = "sugarjazy";
  version = "0.6.0";
  python = python310;

  projectDir = ./.;
  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "sugarjazy";
    rev = "${version}";
    sha256 = "sha256-OxiSWkUkNO0YkkUyYiFZry0hEV2gdydAQlCDcYqfaE4=";
  };
  doCheck = false;

  propagatedBuildInputs = [ python310Packages.dateutil ];

  meta = with lib; {
    homepage = "https://github.com/chmouel/sugarjazy";
    description = "parse json logs nicely";
    license = licenses.asl20;
  };
}
