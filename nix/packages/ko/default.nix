{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.7.2";

  subPackages = [ "cmd/ko" ];
  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "01czn67nj0vyz7f5jq9y559nr58miz1mp82716nyjp12sa90fhgr";
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
