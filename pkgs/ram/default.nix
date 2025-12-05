{
  lib,
  buildGoModule,
  fetchgit,
}:

buildGoModule (finalAttrs: {
  pname = "ram";
  version = "0.3.2";

  src = fetchgit {
    url = "https://git.sr.ht/~vdemeester/ram";
    rev = "v${finalAttrs.version}";
    hash = "sha256-2Vn8alPlVM5j0VSCZwbmnWdZqfd4qp/g29R3lpLhXv4=";
  };
  vendorHash = null;

  meta = {
    description = "A golang opinionated continuous testing tool";
    homepage = "https://git.sr.ht/~vdemeester/ram";
    license = lib.licenses.asl20;
    platforms = lib.platforms.unix;
    mainProgram = "ram";
  };
})
