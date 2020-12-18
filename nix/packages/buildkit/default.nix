{ stdenv, lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  name = "buildkit-${version}";
  version = "0.8.0";
  rev = "v${version}";

  goPackagePath = "github.com/moby/buildkit";
  subPackages = [ "cmd/buildctl" "cmd/buildkitd" ];

  buildFlagsArray = let t = "${goPackagePath}/version"; in
    ''
      -ldflags=
        -X ${t}.Version=${version}
    '';

  src = fetchFromGitHub {
    inherit rev;
    owner = "moby";
    repo = "buildkit";
    sha256 = "0qcgq93wj77i912xqhwrzkzaqz608ilczfn5kcsrf9jk2m1gnx7m";
  };
  vendorSha256 = null;
  doCheck = false;

  meta = {
    description = "concurrent, cache-efficient, and Dockerfile-agnostic builder toolkit";
    homepage = https://github.com/moby/buildkit;
    license = lib.licenses.asl20;
  };
}
