{ stdenv, lib, fetchFromGitHub, removeReferencesTo, go}:

stdenv.mkDerivation rec {
  name = "stellar-${version}";
  version = "0.1.0";
  commit = "ae539df";
  rev = "v${version}";

  src = fetchFromGitHub {
    inherit rev;
    owner = "ehazlett";
    repo = "stellar";
    sha256 = "0gv0z9hf6bh926sga2wadr3bdkigqbl849lhc0552by6c0c8p5dk";
  };

  makeFlags = ["COMMIT=${commit}"];

  buildInputs = [ removeReferencesTo go];

  preConfigure = ''
    # Extract the source
    cd "$NIX_BUILD_TOP"
    mkdir -p "go/src/github.com/ehazlett"
    mv "$sourceRoot" "go/src/github.com/ehazlett/stellar"
    export GOPATH=$NIX_BUILD_TOP/go:$GOPATH
  '';

  preBuild = ''
    cd go/src/github.com/ehazlett/stellar
    patchShebangs .
  '';

  installPhase = ''
    mkdir -p $out/bin
    cp bin/* $out/bin
  '';

  preFixup = ''
    find $out -type f -exec remove-references-to -t ${go} '{}' +
  '';

  meta = {
    description = "Simplified Container Runtime Cluster";
    homepage = "https://github.com/ehazlett/stellar";
    licence = lib.licenses.mit;
  };
}
