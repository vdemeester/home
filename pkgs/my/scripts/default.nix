{
  stdenv,
  lib,
}:

stdenv.mkDerivation {
  pname = "vde-scripts";
  version = "0.4";

  src = ./.;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp $src/bin/* $out/bin/

    runHook postInstall
  '';

  meta = {
    description = "Vincent's personal scripts collection";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
  };
}
