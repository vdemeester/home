{
  python3,
  lib,
  makeWrapper,
  installShellFiles,
}:

python3.pkgs.buildPythonApplication {
  pname = "arr";
  version = "1.1.0";
  format = "other";

  src = ./.;

  nativeBuildInputs = [
    makeWrapper
    installShellFiles
  ];

  propagatedBuildInputs = with python3.pkgs; [
    click
    requests
    spotipy
  ];

  # Don't try to create __pycache__ directories during build
  dontUsePythonImportsCheck = true;

  installPhase = ''
    runHook preInstall

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

    runHook postInstall
  '';

  postFixup = ''
    # Generate shell completions using arr's built-in completion command
    export PYTHONPATH="$out/lib/arr:$PYTHONPATH"
    installShellCompletion --cmd arr \
      --bash <($out/bin/arr completion bash) \
      --fish <($out/bin/arr completion fish) \
      --zsh <($out/bin/arr completion zsh)
  '';

  meta = {
    description = "Unified CLI for managing *arr services (Sonarr, Radarr, Lidarr)";
    longDescription = ''
      arr provides a consistent interface for common operations across
      the *arr media management stack, including renaming, retagging,
      path updates, and Spotify playlist syncing.
    '';
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "arr";
  };
}
