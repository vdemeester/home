{ stdenv, lib, fetchFromGitHub, tzdata }:

stdenv.mkDerivation rec {
  pname = "batzconverter";
  version = "1.0.3";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = "${version}";
    sha256 = "0bsmh0wzqbdm3b800gl9fz1vwyfm2xpgqs3smkgb17b448wm2jsj";
  };

  installPhase = ''
    mkdir -p $out/bin
    # Some other hard-coded paths to fix:
    sed -i 's#/usr/share/zoneinfo/#${tzdata}/share/zoneinfo/#g' \
      batz.sh
    cp batz.sh $out/bin/batz
    chmod +x $out/bin/batz
  '';
}
