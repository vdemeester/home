{ stdenv, lib, fetchFromGitHub, tzdata }:

stdenv.mkDerivation rec {
  pname = "batzconverter";
  version = "2.0.0";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = "${version}";
    sha256 = "sha256-z6ue3cGX0NyUVauZF3X5688Rocuk9gCETUsZA6PYyT0=";
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
