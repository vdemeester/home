{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.5.2";

  goPackagePath = "github.com/google/ko";

  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "034sz1idnipd75xx3vdmlahz7j615xccbbmnck2vd80sixbm9qlw";
  };

  meta = with stdenv.lib; {
    homepage = https://github.com/google/ko;
    description = "Build and deploy Go applications on Kubernetes";
    license = licenses.asl20;
    maintainers = with maintainers; [ vdemeester ];
  };
}
