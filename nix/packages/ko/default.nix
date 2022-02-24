{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.10.0";

  subPackages = [ "cmd/ko" ];
  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "sha256-Xhe5WNHQ+Oa1m/6VwC3zCwWzXRc1spSfPp4jySsOcuU=";
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
