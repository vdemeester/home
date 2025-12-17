{
  lib,
  buildPythonPackage,
  fetchPypi,
  poetry-core,
  mediafile,
  reflink,
  toml,
  typeguard,
}:

buildPythonPackage rec {
  pname = "beets-filetote";
  version = "1.1.1";
  pyproject = true;

  src = fetchPypi {
    pname = "beets_filetote";
    inherit version;
    hash = "sha256-2u9Zhlwr/R7Q2Vxr4bs0B58lADg1n7qao8WRwRttCnk=";
  };

  postPatch = ''
    substituteInPlace pyproject.toml --replace-fail "poetry-core<2.0.0" "poetry-core"
  '';

  build-system = [
    poetry-core
  ];

  dependencies = [
    mediafile
    reflink
    toml
    typeguard
  ];

  # Don't check for beets dependency - it will be provided by the beets package
  pythonRemoveDeps = [ "beets" ];

  # Remove conflicting files from namespace package
  postInstall = ''
    rm -rf $out/lib/python*/site-packages/beetsplug/__init__.py
    rm -rf $out/lib/python*/site-packages/beetsplug/__pycache__
  '';

  # Tests require a running beets setup
  doCheck = false;

  # Skip imports check - beets tries to create config dirs during import
  pythonImportsCheck = [ ];

  meta = with lib; {
    description = "Beets plugin to copy/move non-music extra files, attachments, and artifacts during import";
    homepage = "https://github.com/gtronset/beets-filetote";
    license = licenses.mit;
    maintainers = with maintainers; [ ];
  };
}
