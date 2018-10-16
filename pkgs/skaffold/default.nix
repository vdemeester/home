{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "skaffold-${version}";
  version = "0.16.0";
  rev = "v${version}";

  goPackagePath = "github.com/GoogleContainerTools/skaffold";
  subPackages = ["cmd/skaffold"];

  src = fetchFromGitHub {
    inherit rev;
    owner = "GoogleContainerTools";
    repo = "skaffold";
    sha256 = "0vpjxyqppyj4zs02n8b0k0qd8zidrrcks60x6qd5a4bbqa0c1zld";
  };

  meta = {
    description = "Easy and Repeatable Kubernetes Development";
    homepage = "https://github.com/GoogleContainerTools/skaffold";
    licence = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
  };
}
