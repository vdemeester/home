{
  stdenv,
  lib,
  fetchFromGitHub,
  tzdata,
}:

stdenv.mkDerivation (finalAttrs: {
  pname = "batzconverter";
  version = "2.8.0";

  src = fetchFromGitHub {
    owner = "chmouel";
    repo = "batzconverter";
    rev = finalAttrs.version;
    sha256 = "sha256-9tN0fr1FcAxBRDpV5l7N6iAQ+1WOb6gEbpcmahfta5o=";
  };

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    # Some other hard-coded paths to fix:
    sed -i 's#/usr/share/zoneinfo/#${tzdata}/share/zoneinfo/#g' \
      batz.sh
    cp batz.sh $out/bin/batz
    chmod +x $out/bin/batz

    runHook postInstall
  '';

  meta = {
    description = "Convert Strava .fit or .tcx file to BATZ format";
    homepage = "https://github.com/chmouel/batzconverter";
    license = lib.licenses.asl20;
    platforms = lib.platforms.unix;
    mainProgram = "batz";
  };
})
