{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.9.1";

  subPackages = [ "cmd/ko" ];
  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "178pj9b59lwafys82z8qvb0knd5bzvibc9sy0d5q76kbvajk2842";
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
