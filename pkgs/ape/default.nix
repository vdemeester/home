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
  vendorSha256 = "1zanxsbxhm0dpk2q94fp2rx2x1i8r3j28piz86k3k4vvqnykyvj1";
  modSha256 = "${vendorSha256}";

  meta = {
    description = "a git mirror *upstream* updater ";
    homepage = "https://git.sr.ht/~vdemeester/ape";
    license = lib.licenses.asl20;
  };
}
