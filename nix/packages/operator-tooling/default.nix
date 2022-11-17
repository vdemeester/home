{ stdenv, lib, buildGoModule, git, fetchFromGitHub }:

buildGoModule rec {
  name = "operator-tool-${version}";
  version = "0.0.1";
  rev = "v${version}";

  src = fetchFromGitHub {
    inherit rev;
    owner = "openshift-pipelines";
    repo = "operator-tooling";
    sha256 = "sha256-QCtX1fgJ5HVafn3FmiEjah+AF+po3ZQh7kODrjc2On4=";
  };
  vendorSha256 = null;

  meta = {
    description = "Tooling for managing operator remote payload";
    homepage = https://github.com/openshift-pipelines/operator-tooling;
    license = lib.licenses.asl20;
  };
}
