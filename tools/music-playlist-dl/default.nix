{
  lib,
  python3,
  yt-dlp,
}:

python3.pkgs.buildPythonApplication {
  pname = "music-playlist-dl";
  version = "1.0.0";
  format = "other";

  src = ./.;

  propagatedBuildInputs = with python3.pkgs; [
    pyyaml
  ];

  # yt-dlp is a runtime dependency
  makeWrapperArgs = [
    "--prefix PATH : ${lib.makeBinPath [ yt-dlp ]}"
  ];

  # Don't unpack since we're not using a typical Python package structure
  dontUnpack = true;
  dontBuild = true;

  # Simple install: just copy the script
  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin
    cp ${./music-playlist-dl.py} $out/bin/music-playlist-dl
    chmod +x $out/bin/music-playlist-dl

    runHook postInstall
  '';

  meta = with lib; {
    description = "Download DJ podcasts/radio shows and generate playlists";
    homepage = "https://github.com/vdemeester/home";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "music-playlist-dl";
  };
}
