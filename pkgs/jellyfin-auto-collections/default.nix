{
  lib,
  python3,
  fetchFromGitHub,
  makeWrapper,
}:

python3.pkgs.buildPythonApplication rec {
  pname = "jellyfin-auto-collections";
  version = "unstable-2024-01-01";

  format = "other"; # No setup.py or pyproject.toml

  src = fetchFromGitHub {
    owner = "ghomasHudson";
    repo = "Jellyfin-Auto-Collections";
    rev = "master";
    hash = "sha256-4dfpOgZ6mCbyzFKeP53ZQpfjK1dOp45rsclQzoQjzeQ=";
  };

  nativeBuildInputs = [ makeWrapper ];

  propagatedBuildInputs = with python3.pkgs; [
    apscheduler
    beautifulsoup4
    certifi
    charset-normalizer
    loguru
    pluginlib
    pyaml-env
    pytz
    pyyaml
    requests
    requests-cache
    setuptools
    six
    soupsieve
    tzlocal
    urllib3
    url-normalize
    numpy
    packaging
    pillow
    pyparsing
    python-dateutil
    attrs
    cattrs
    platformdirs
  ];

  dontBuild = true;
  dontCheck = true;

  installPhase = ''
    runHook preInstall

    mkdir -p $out/bin $out/lib/jellyfin-auto-collections
    cp -r * $out/lib/jellyfin-auto-collections/

    # Create wrapper script with proper PYTHONPATH
    makeWrapper ${python3}/bin/python $out/bin/jellyfin-auto-collections \
      --add-flags "$out/lib/jellyfin-auto-collections/main.py" \
      --prefix PYTHONPATH : "$PYTHONPATH:$out/lib/jellyfin-auto-collections"

    runHook postInstall
  '';

  meta = with lib; {
    description = "Automatically make jellyfin collections from IMDB, Letterboxd lists and more";
    homepage = "https://github.com/ghomasHudson/Jellyfin-Auto-Collections";
    license = licenses.mit;
    maintainers = [ ];
    mainProgram = "jellyfin-auto-collections";
  };
}
