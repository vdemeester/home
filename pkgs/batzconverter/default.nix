{ stdenv, lib, fetchFromGitHub }:

stdenv.mkDerivation rec {
  pname = "batzconverter";
  version = "git";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = "907a1a6f97c590c5d6f9427c7a92e6be1d18a697";
    sha256 = "0alrxqhbi3ivx6a2n443rmry3y9b896lansmvfw6xwq7p1h7x22y";
  };

  installPhase = ''
    mkdir -p $out/bin
    cp batz.sh $out/bin/batz
    chmod +x $out/bin/batz
  '';
}
