{ stdenv, lib, buildGoModule, fetchgit }:

buildGoModule rec {
  name = "ram-${version}";
  version = "0.3.1";
  rev = "v${version}";

  src = fetchgit {
    inherit rev;
    url = "https://git.sr.ht/~vdemeester/ram";
    sha256 = "1xmx4dc30din9cwl7c0zz9pvdzk9mirvps8m89xfrkmcj5h8k8rr";
  };
  vendorSha256 = "16b2061x0z83c4j59fxm1hhzr9akq1y2lbpiqdh139sqy6l6h7df";
  modSha256 = "${vendorSha256}";

  meta = {
    description = "A golang opiniated continuous testing tool 🐏";
    homepage = "https://git.sr.ht/~vdemeester/ram";
    license = lib.licenses.asl20;
  };
}
