{
  buildGoModule,
  fetchgit,
  lib,
}:

buildGoModule (finalAttrs: {
  pname = "govanityurl";
  version = "0.1.0";

  src = fetchgit {
    url = "https://git.sr.ht/~vdemeester/vanityurl";
    rev = "v${finalAttrs.version}";
    hash = "sha256-7AdNbbIcNSPRAi8u0+2b/Lscq4MFXci0+WeND8wZkhU=";
  };
  vendorHash = "sha256-qe7SxvrmgbcUnBUbUVx/l3hLZ1BRHZyDgi8tLtULCms=";

  meta = {
    description = "Go vanity URL server for custom import paths";
    homepage = "https://git.sr.ht/~vdemeester/vanityurl";
    license = lib.licenses.asl20;
    maintainers = with lib.maintainers; [ vdemeester ];
    platforms = lib.platforms.unix;
    mainProgram = "govanityurl";
  };
})
