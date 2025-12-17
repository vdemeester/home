{
  lib,
  buildPythonPackage,
  fetchPypi,
}:

buildPythonPackage rec {
  pname = "beets-lidarr-fields";
  version = "1.1.2";
  format = "setuptools";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-wlybw3v0QLfeIgbezz5PQdbGFsu9KLI1AkVQD4CrgI4=";
  };

  # Don't check for beets dependency - it will be provided by the beets package
  pythonRemoveDeps = [ "beets" ];

  # Remove conflicting files from namespace package
  postInstall = ''
    rm -rf $out/lib/python*/site-packages/beetsplug/__init__.py
    rm -rf $out/lib/python*/site-packages/beetsplug/__pycache__
  '';

  # Tests require a running beets setup
  doCheck = false;

  # Skip imports check to avoid needing beets at build time
  pythonImportsCheck = [ ];

  meta = with lib; {
    description = "Beets plugin that defines useful template fields to customize path formats in a Lidarr-compatible way";
    homepage = "https://github.com/rafaelparente/beets-lidarr-fields";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
