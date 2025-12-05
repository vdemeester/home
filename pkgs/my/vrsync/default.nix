{
  stdenv,
  lib,
}:

stdenv.mkDerivation {
  pname = "vrsync";
  version = "0.1.0";

  src = ./.;

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp $src/vrsync $out/bin
    chmod +x $out/bin/vrsync

    runHook postInstall
  '';

  meta = {
    description = "Vincent's rsync wrapper utility";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "vrsync";
  };
}
