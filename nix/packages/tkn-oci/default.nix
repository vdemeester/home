{ stdenv, lib, buildGoModule, fetchFromGitHub }:
let
  version = "unstable-20201030";

  buildOci = src: buildGoModule {
    inherit src version;
    pname = "oci";
    vendorSha256 = "0l1fi9dgj19shmsc4hc3zpnakxygz584dl8yqya6nw6cpbn9jlg7";
  };
in
stdenv.mkDerivation rec {
  pname = "tkn-oci";
  name = "${pname}-${version}";

  src = fetchFromGitHub {
    owner = "tektoncd";
    repo = "experimental";
    rev = "4956a01c6aee52e5f204913e91e2526442bda34d";
    sha256 = "1y59z30qiqbahb9fkikgrgx9gpig9gylb0wwjq1ysbdnwmqdwpxd";
  };
  installPhase = ''
    mkdir -p $out/bin
    cp ${buildOci "${src}/oci"}/bin/oci $out/bin/tkn-oci
  '';

  meta = with lib; {
    homepage = https://github.com/tektoncd/experimental;
    description = "Experimental tool to package tekton resources in oci image";
    license = licenses.asl20;
    maintainers = with maintainers; [ vdemeester ];
  };
}
