{ stdenv, lib, buildGoModule, fetchgit }:

buildGoModule rec {
  name = "ape-${version}";
  version = "0.4.2";
  rev = "v${version}";

  src = fetchgit {
    inherit rev;
    url = "https://git.sr.ht/~vdemeester/ape";
    sha256 = "0f2rfmpqy66ris4gqh58dr9qvz8fmskldh41kpniy5vr0jxqf3av";
  };
  vendorHash = "1frjpg21jm543idja5yj96bl5wda1vpr1cp4vds2pwxnkg267d2w";

  meta = {
    description = "a git mirror *upstream* updater ";
    homepage = "https://git.sr.ht/~vdemeester/ape";
    license = lib.licenses.asl20;
  };
}
