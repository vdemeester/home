{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule rec {
  name = "manifest-tool-${version}";
  version = "2.0.0";
  rev = "v${version}";

  subPackages = [ "cmd/manifest-tool" ];
  modRoot = "./v2";

  src = fetchFromGitHub {
    inherit rev;
    owner = "estesp";
    repo = "manifest-tool";
    sha256 = "sha256-KBX/VgFKm3xT1tRxKKIO3u4JZ3htkTVVnB1GqpJ0xO0=";
  };
  vendorHash = null;

  meta = {
    description = "";
    homepage = "https://github.com/estesp/manifest-tool";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
  };

}
