{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.6.2";

  goPackagePath = "github.com/google/ko";

  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "0r8lwr431zlf04yr8avaw7kxf4bz0hrrdv493knla66qbyzj9fsx";
  };
  vendorSha256 = null;
  # TestGoBuild{,Index} doesn't work because it assumes a .git
  doCheck = false;

  meta = with stdenv.lib; {
    homepage = https://github.com/google/ko;
    description = "Build and deploy Go applications on Kubernetes";
    license = licenses.asl20;
    maintainers = with maintainers; [ vdemeester ];
  };
}
