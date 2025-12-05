{
  stdenv,
  lib,
}:

stdenv.mkDerivation rec {
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

  meta = with lib; {
    description = "Vincent's rsync wrapper utility";
    license = licenses.mit;
    platforms = platforms.unix;
    mainProgram = "vrsync";
  };
}
