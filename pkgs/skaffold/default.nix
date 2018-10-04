{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "skaffold-${version}";
  version = "0.15.1";
  rev = "v${version}";

  goPackagePath = "github.com/GoogleContainerTools/skaffold";
  subPackages = ["cmd/skaffold"];

  src = fetchFromGitHub {
    inherit rev;
    owner = "GoogleContainerTools";
    repo = "skaffold";
    sha256 = "1ckkibagcxdbwsrfniailq6sdrs24vg8kbv9w26hhgas2l82a1xr";
  };

  meta = {
    description = "Easy and Repeatable Kubernetes Development";
    homepage = "https://github.com/GoogleContainerTools/skaffold";
    licence = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
  };
}
