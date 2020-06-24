{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "batzconverter";
  version = "1.0";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = "${version}";
    sha256 = "0alrxqhbi3ivx6a2n443rmry3y9b896lansmvfw6xwq7p1h7x22y";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp batz.sh $out/bin/batz
    chmod +x $out/bin/batz
  '';
}
