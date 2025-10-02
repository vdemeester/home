{
  stdenv,
  fetchFromGitHub,
  tzdata,
}:

stdenv.mkDerivation rec {
  pname = "batzconverter";
  version = "2.8.0";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = "${version}";
    sha256 = "sha256-9tN0fr1FcAxBRDpV5l7N6iAQ+1WOb6gEbpcmahfta5o=";
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
