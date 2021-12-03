{ stdenv, lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  name = "buildkit-${version}";
  version = "0.9.3";
  rev = "v${version}";

  subPackages = [ "cmd/buildctl" "cmd/buildkitd" ];

  ldflags = [
    "-X github.com/moby/buildkit/version.Version=${version}"
  ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "moby";
    repo = "buildkit";
    sha256 = "sha256-xjuHMjJjA4sx2Hrr6tPpvKtSmhGZ3AZka733DLxmYfk=";
  };
  vendorSha256 = null;
  doCheck = false;

  meta = {
    description = "concurrent, cache-efficient, and Dockerfile-agnostic builder toolkit";
    homepage = https://github.com/moby/buildkit;
    license = lib.licenses.asl20;
  };
}
