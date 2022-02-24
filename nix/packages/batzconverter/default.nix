{ stdenv, lib, fetchFromGitHub, tzdata }:

stdenv.mkDerivation rec {
  pname = "batzconverter";
  version = "1.2.1";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = "${version}";
    sha256 = "sha256-YAcCtek4pYvhgUaS8EZT0kFBLZQOFPCaxKh8bNr+ru4=";
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
