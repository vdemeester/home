{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "manifest-tool-${version}";
  version = "unstable-2020-10-26";
  #rev = "v${version}";
  rev = "bae5531170d45955c2d72d1b29d77ce1b0c9dedb";

  subPackages = [ "cmd/manifest-tool" ];

  src = fetchFromGitHub {
    inherit rev;
    owner = "estesp";
    repo = "manifest-tool";
    sha256 = "066ls7sxacfy236c9kjbhhqsmpbiy8wx4ishyw6nxi4n5zvzs3kl";
  };
  vendorSha256 = null;

  meta = {
    description = "";
    homepage = "https://github.com/estesp/manifest-tool";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
  };

}
