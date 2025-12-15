{
  python3,
  lib,
  makeWrapper,
  fzf,
  aria2,
  wget,
}:

python3.pkgs.buildPythonApplication {
  pname = "download-kiwix-zim";
  version = "1.0.0";
  format = "other";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  # Runtime dependencies
  buildInputs = [
    fzf
    aria2
    wget
  ];

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin

    # Install the script
    cp download-kiwix-zim $out/bin/download-kiwix-zim
    chmod +x $out/bin/download-kiwix-zim

    # Wrap the script to ensure dependencies are in PATH
    wrapProgram $out/bin/download-kiwix-zim \
      --prefix PATH : ${
        lib.makeBinPath [
          fzf
          aria2
          wget
        ]
      }

    runHook postInstall
  '';

  meta = {
    description = "Browse and download ZIM files from the Kiwix library using fzf";
    longDescription = ''
      Interactive tool to browse the Kiwix catalog and download offline
      content archives (ZIM files) for Wikipedia and other educational
      resources. Features multi-select with fzf and parallel downloads
      with aria2c.
    '';
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "download-kiwix-zim";
  };
}
