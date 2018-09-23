{ stdenv, lib, fetchFromGitHub, removeReferencesTo
, go, btrfs-progs }:

with lib;

stdenv.mkDerivation rec {
  name = "containerd-${version}";
  version = "1.2.0-rc.0";

  src = fetchFromGitHub {
    owner = "containerd";
    repo = "containerd";
    rev = "v${version}";
    sha256 = "01y21fx5aidxrn3xz562sqyp9anw85hv9cbpbknj3wf2w15lmkhy";
  };

  hardeningDisable = [ "fortify" ];

  buildInputs = [ removeReferencesTo go btrfs-progs ];
  buildFlags = "VERSION=v${version}";

  BUILDTAGS = []
    ++ optional (btrfs-progs == null) "no_btrfs";

  preConfigure = ''
    # Extract the source
    cd "$NIX_BUILD_TOP"
    mkdir -p "go/src/github.com/containerd"
    mv "$sourceRoot" "go/src/github.com/containerd/containerd"
    export GOPATH=$NIX_BUILD_TOP/go:$GOPATH
'';

  preBuild = ''
    cd go/src/github.com/containerd/containerd
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
    homepage = https://containerd.io/;
    description = "A daemon to control runC";
    license = licenses.asl20;
    maintainers = with maintainers; [ offline vdemeester ];
    platforms = platforms.linux;
  };
}
