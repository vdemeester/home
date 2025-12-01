{
  python3,
  lib,
  makeWrapper,
}:

python3.pkgs.buildPythonApplication {
  pname = "arr";
  version = "1.1.0";
  format = "other";

  src = ./.;

  nativeBuildInputs = [ makeWrapper ];

  propagatedBuildInputs = with python3.pkgs; [
    click
    requests
    spotipy
  ];

  # Don't try to create __pycache__ directories during build
  dontUsePythonImportsCheck = true;

  installPhase = ''
    mkdir -p $out/bin $out/lib/arr

    # Install the main CLI
    cp arr $out/bin/arr
    chmod +x $out/bin/arr

    # Install the library
    cp lib.py $out/lib/arr/

    # Install commands
    cp -r commands $out/lib/arr/

    # Create __init__.py for the package
    touch $out/lib/arr/__init__.py

    # Wrap the main script to set PYTHONPATH
    wrapProgram $out/bin/arr \
      --prefix PYTHONPATH : "$out/lib/arr"
  '';

  meta = with lib; {
    description = "Unified CLI for managing *arr services (Sonarr, Radarr, Lidarr)";
    longDescription = ''
      arr provides a consistent interface for common operations across
      the *arr media management stack, including renaming, retagging,
      path updates, and Spotify playlist syncing.
    '';
    platforms = platforms.unix;
  };
}
