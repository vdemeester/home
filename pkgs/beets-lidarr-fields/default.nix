{
  lib,
  python3Packages,
  fetchPypi,
}:

python3Packages.buildPythonPackage rec {
  pname = "beets-lidarr-fields";
  version = "1.1.2";
  format = "setuptools";

  src = fetchPypi {
    inherit pname version;
    hash = "sha256-wlybw3v0QLfeIgbezz5PQdbGFsu9KLI1AkVQD4CrgI4=";
  };

  propagatedBuildInputs = with python3Packages; [
    beets-minimal
  ];

  # Tests require a running beets setup
  doCheck = false;

  pythonImportsCheck = [
    "beetsplug.lidarrfields"
  ];

  meta = with lib; {
    description = "Beets plugin that defines useful template fields to customize path formats in a Lidarr-compatible way";
    homepage = "https://github.com/rafaelparente/beets-lidarr-fields";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
