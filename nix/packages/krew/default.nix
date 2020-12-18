{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "krew-${version}";
  version = "0.3.4";
  rev = "v${version}";

  goPackagePath = "github.com/kubernetes-sigs/krew";
  subPackages = [ "cmd/krew" ];
  src = fetchFromGitHub {
    inherit rev;
    owner = "kubernetes-sigs";
    repo = "krew";
    sha256 = "0n10kpr2v9jzkz4lxrf1vf9x5zql73r5q1f1llwvjw6mb3xyn6ij";
  };
  vendorSha256 = "01jy73g087bng5xhbwd7pigsd44jl7pdfwf7ff43m9jsziknd39i";

  meta = {
    description = "The package manager for 'kubectl plugins. ";
    homepage = "https://github.com/kubernetes-sigs/krew";
    license = lib.licenses.asl20;
  };
}
