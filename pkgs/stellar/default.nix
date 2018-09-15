{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "stellar-${version}";
  version = "0.1.0";
  commit = "ae539df7b6796565a77365252350450113682e0b";
  rev = "v${version}";

  goPackagePath = "github.com/ehazlett/stellar";

  buildFlagsArray = [''-ldflags=
    -X github.com/$(REPO)/version.GitCommit=${commit}
    -X github.com/$(REPO)/version.Build=${version}
  ''];
  /*
  subPackages = [
    "cmd/etcd"
    "cmd/etcdctl"
  ];
  */

  src = fetchFromGitHub {
    inherit rev;
    owner = "ehazlett";
    repo = "stellar";
    sha256 = "0gv0z9hf6bh926sga2wadr3bdkigqbl849lhc0552by6c0c8p5dk";
  };

  meta = {
    description = "Simplified Container Runtime Cluster";
    homepage = "https://github.com/ehazlett/stellar";
    licence = lib.licenses.mit;
  };
}
