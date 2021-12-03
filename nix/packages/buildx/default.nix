{ stdenv, lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  name = "buildx-${version}";
  version = "0.7.1";
  rev = "v${version}";

  subPackages = [ "cmd/buildx" ];

  ldflags = [
    "-X github.com/docker/buildx/version.Version=${version}"
  ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "docker";
    repo = "buildx";
    sha256 = "sha256-5EV0Rw1+ufxQ1wmQ0EJXQ7HVtXVbB4do/tet0QFRi08=";
  };
  vendorSha256 = null;
  doCheck = false;

  meta = {
    description = "Docker CLI plugin for extended build capabilities with buildkit";
    homepage = https://github.com/docker/buildx;
    license = lib.licenses.asl20;
  };
}
