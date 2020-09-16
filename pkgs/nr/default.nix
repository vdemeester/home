{ stdenv, lib, buildGoModule, fetchgit }:

buildGoModule rec {
  name = "nr-${version}";
  version = "0.5.0";
  rev = "v${version}";

  src = fetchgit {
    inherit rev;
    url = "https://git.sr.ht/~vdemeester/nr";
    sha256 = "0b8y5wsy8f0r9aspn1045nrpkph9kbh6754m2kkyx4i9zjhgnqjp";
  };
  vendorSha256 = "17cz2gahs1j9vd9nqg36q2q04xq24gd2pyvivxkjhqgmq2fcpl17";
  modSha256 = "${vendorSha256}";

  meta = {
    description = "a nix run alias generator";
    homepage = "https://git.sr.ht/~vdemeester/nr";
    license = lib.licenses.asl20;
  };
}
