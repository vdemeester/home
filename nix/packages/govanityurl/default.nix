{ stdenv, lib, buildGoModule, fetchgit }:

buildGoModule rec {
  pname = "govanityurl";
  name = "${pname}-${version}";
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~vdemeester/vanityurl";
    rev = "v${version}";
    sha256 = "05cj3760z3b7z6schp85hfmirfzwkgnx6big0b8j6d8wn9nls1zc";
  };
  vendorHash = "0sqa1gajwb9gha1rq7aia1klny4pgxf526qmkhabg0g6zb3d5vm9";
}
