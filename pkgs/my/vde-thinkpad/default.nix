{
  lib,
  stdenv,
}:

stdenv.mkDerivation {
  pname = "vde-thinkpad";
  version = "0.1.0";

  src = ./.;

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp $src/dock $out/bin
    chmod +x $out/bin/dock

    runHook postInstall
  '';

  meta = {
    description = "ThinkPad dock management script";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "dock";
  };
}
