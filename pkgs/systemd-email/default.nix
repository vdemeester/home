{
  stdenv,
  lib,
}:

stdenv.mkDerivation rec {
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

  meta = with lib; {
    description = "Systemd service for sending email notifications on service failures";
    license = licenses.mit;
    platforms = platforms.linux;
    mainProgram = "systemd-email";
  };
}
