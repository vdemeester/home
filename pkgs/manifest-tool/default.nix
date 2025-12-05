{
  lib,
  buildGoModule,
  fetchFromGitHub,
}:

buildGoModule (finalAttrs: {
  pname = "manifest-tool";
  version = "2.2.1";

  subPackages = [ "cmd/manifest-tool" ];
  modRoot = "./v2";

  src = fetchFromGitHub {
    owner = "estesp";
    repo = "manifest-tool";
    rev = "v${finalAttrs.version}";
    hash = "sha256-aw8c8VhSFexUpQqXDOd/pRSiuRl4njBe+LDONTVK7Uw=";
  };
  vendorHash = null;

  meta = {
    description = "Tool for inspecting and creating multi-platform container image manifests";
    homepage = "https://github.com/estesp/manifest-tool";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
    platforms = lib.platforms.unix;
    mainProgram = "manifest-tool";
  };
})
