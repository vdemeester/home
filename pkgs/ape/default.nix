{
  lib,
  buildGoModule,
  fetchgit,
}:

buildGoModule (finalAttrs: {
  pname = "ape";
  version = "0.4.2";

  src = fetchgit {
    url = "https://git.sr.ht/~vdemeester/ape";
    rev = "v${finalAttrs.version}";
    sha256 = "sha256-Ww2HuwR5Fx/tnYHARqeuDv2NU26oQPyIjtkYj291WTg=";
  };
  vendorHash = "sha256-XLRjxJu28yt02+SykO8OqvFCl0nSFyVbHKRUGcS7Mrs=";

  meta = {
    description = "A git mirror upstream updater";
    homepage = "https://git.sr.ht/~vdemeester/ape";
    license = lib.licenses.asl20;
    platforms = lib.platforms.unix;
    mainProgram = "ape";
  };
})
