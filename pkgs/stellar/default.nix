{ stdenv, lib, fetchFromGitHub, removeReferencesTo, go}:

stdenv.mkDerivation rec {
  name = "stellar-${version}";
  version = "0.1.0";
  commit = "92a8e36";
  #commit = "ae539df";
  #rev = "v${version}";
  rev = "92a8e365c417dfdbc1df557fc4000c15fe955027";

  src = fetchFromGitHub {
    inherit rev;
    #owner = "ehazlett";
    #repo = "stellar";
    owner = "vdemeester";
    repo = "stellar";
    sha256 = "024vg6lrwhp6j3zqwswgvbr043inijnmzyc4jhn6vgdwalf33k10";
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
