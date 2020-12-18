{ stdenv, lib, fetchFromGitHub, buildGoModule }:

buildGoModule rec {
  name = "buildx-${version}";
  version = "0.5.1";
  rev = "v${version}";

  goPackagePath = "github.com/docker/buildx";
  subPackages = [ "cmd/buildx" ];

  buildFlagsArray = let t = "${goPackagePath}/version"; in
    ''
      -ldflags=
        -X ${t}.Version=${version}
    '';

  src = fetchFromGitHub {
    inherit rev;
    owner = "docker";
    repo = "buildx";
    sha256 = "0l03ncs1x4lhgy0kf7bd1zq00md8fi93f8xq6k0ans4400divfzk";
  };
  vendorSha256 = null;
  doCheck = false;

  meta = {
    description = "Docker CLI plugin for extended build capabilities with buildkit";
    homepage = https://github.com/docker/buildx;
    license = lib.licenses.asl20;
  };
}
