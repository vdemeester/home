{
  python3,
  lib,
  makeWrapper,
  rsync,
  openssh,
}:

python3.pkgs.buildPythonApplication {
  pname = "jellyfin-favorites-sync";
  version = "1.0.0";
  format = "other";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  propagatedBuildInputs = with python3.pkgs; [
    requests
    click
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp jellyfin-favorites-sync $out/bin/
    chmod +x $out/bin/jellyfin-favorites-sync

    # Wrap to add arr lib to PYTHONPATH and rsync/ssh to PATH
    wrapProgram $out/bin/jellyfin-favorites-sync \
      --prefix PYTHONPATH : "${../arr}" \
      --prefix PATH : "${
        lib.makeBinPath [
          rsync
          openssh
        ]
      }"

    runHook postInstall
  '';

  meta = {
    description = "Sync Jellyfin favorite items to remote host via rsync";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "jellyfin-favorites-sync";
  };
}
