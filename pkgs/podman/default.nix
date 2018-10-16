{ stdenv, lib, fetchFromGitHub, makeWrapper, removeReferencesTo, pkgconfig
, go, libtool, gpgme, lvm2
, btrfs-progs, libseccomp, gcc
}:

stdenv.mkDerivation rec {
  name = "podman-${version}";
  version = "0.10.1";
  src = fetchFromGitHub {
    owner = "containers";
    repo = "libpod";
    rev = "v${version}";
    sha256 = "0156aqdza7kqd4i42n81dcpv03yll92151ggsziklslz8brwc7yk";
  };
    # Optimizations break compilation of libseccomp c bindings
    hardeningDisable = [ "fortify" ];

    nativeBuildInputs = [ pkgconfig ];
    buildInputs = [
      makeWrapper removeReferencesTo go libtool
      btrfs-progs libseccomp gcc gpgme lvm2
      ];

    dontStrip = true;

    buildPhase = ''
    patchShebangs .
    mkdir -p .gopath/src/github.com/containers
    ln -sf $PWD .gopath/src/github.com/containers/libpod
    ln -sf $PWD/vendor/github.com/varlink .gopath/src/github.com/varlink
    export GOPATH="$PWD/.gopath:$GOPATH"
    make binaries
    '';

    installPhase = ''
    install -Dm755 bin/podman $out/bin/podman
    '';

    outputs = ["out"];

    preFixup = ''
      find $out -type f -exec remove-references-to -t ${go} -t ${stdenv.cc.cc} '{}' +
      find $out -type f -exec remove-references-to -t ${stdenv.glibc.dev} '{}' +
    '';

}
