{ stdenv, lib, buildGoPackage, fetchgit }:

buildGoPackage rec {
  name = "kubespy-unstable-${version}";
  version = "2018-09-27";
  rev = "9cd039c238f3eb707a2918a52289e3eda6b9452c";

  goPackagePath = "github.com/pulumi/kubespy";

  src = fetchgit {
    inherit rev;
    url = "https://github.com/pulumi/kubespy.git";
    sha256 = "1prr310nm1gf2v1l0v5lgkv9gkvfrslvkfxwy52g3w1kk54clfqf";
  };

  goDeps = ./deps.nix;

  meta = {
  };
}
