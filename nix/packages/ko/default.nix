{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.8.3";

  subPackages = [ "cmd/ko" ];
  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "05q8cclf229b29p9d6cg17357cmhqa6gxqbh9f9b25rfirjrg0rf";
  };
  vendorSha256 = null;
  # TestGoBuild{,Index} doesn't work because it assumes a .git
  doCheck = false;

  meta = with lib; {
    homepage = https://github.com/google/ko;
    description = "Build and deploy Go applications on Kubernetes";
    license = licenses.asl20;
    maintainers = with maintainers; [ vdemeester ];
  };
}
