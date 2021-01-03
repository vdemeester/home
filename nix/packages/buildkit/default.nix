{ stdenv, lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  name = "buildkit-${version}";
  version = "0.8.0";
  rev = "v${version}";

  subPackages = [ "cmd/buildctl" "cmd/buildkitd" ];

  buildFlagsArray = let t = "github.com/moby/buildkit/version"; in
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
