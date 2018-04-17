{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  name = "envbox-${version}";
  version = "0.0.3";
  rev = "v${version}";

  goPackagePath = "github.com/justone/envbox";

  src = fetchFromGitHub {
    inherit rev;
    owner = "justone";
    repo = "envbox";
    sha256 = "0ihwdhq05s0116ngcnh3vsxhm5k3lnidyaky2qv6dbw1hnq2pv1y";
  };

  meta = {
    description = "Secure environment variables via secretbox";
    homepage = "https://github.com/justone/envbox";
    licence = lib.licenses.mit;
  };
}
