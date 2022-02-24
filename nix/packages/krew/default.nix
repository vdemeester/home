{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "krew-${version}";
  version = "0.4.3";
  rev = "v${version}";

  subPackages = [ "cmd/krew" ];
  src = fetchFromGitHub {
    inherit rev;
    owner = "kubernetes-sigs";
    repo = "krew";
    sha256 = "sha256-aW9yASskwDt+5Lvsdju9ZR/HeZ4x8heWljdhqK0ZTx8=";
  };
  vendorSha256 = "sha256-P2FcCznzsGWYdya+vMLMKFXhZX0f0ZpHk00vuVbWMko=";

  meta = {
    description = "The package manager for 'kubectl plugins. ";
    homepage = "https://github.com/kubernetes-sigs/krew";
    license = lib.licenses.asl20;
  };
}
