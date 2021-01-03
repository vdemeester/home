{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "athens-${version}";
  version = "0.10.0";
  rev = "v${version}";

  subPackages = [ "cmd/proxy" ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "gomods";
    repo = "athens";
    sha256 = "10l96v2ayz3bp7kzr3a2lwyb95hc3dymlvcanl4629dy087ap6zj";
  };
  vendorSha256 = "1sjahs7a06vsnzfbwgzh6wdqwlcgpq5w4zkhf711ws4njipkmq1r";

  meta = {
    description = "a Go module datastore and proxy";
    homepage = "https://github.com/godmods/athens";
    license = lib.licenses.mit;
  };
}
