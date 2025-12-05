{
  stdenv,
  lib,
}:

stdenv.mkDerivation {
  pname = "systemd-email";
  version = "0.1.0";

  src = ./.;

  dontUnpack = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp $src/systemd-email $out/bin
    chmod +x $out/bin/systemd-email

    runHook postInstall
  '';

  meta = {
    description = "Systemd service for sending email notifications on service failures";
    license = lib.licenses.mit;
    platforms = lib.platforms.linux;
    mainProgram = "systemd-email";
  };
}
