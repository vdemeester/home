{ stdenv, lib, buildGoPackage, fetchFromGitHub }:

buildGoPackage rec {
  pname = "ko";
  name = "${pname}-${version}";
  version = "0.6.0";

  goPackagePath = "github.com/google/ko";

  src = fetchFromGitHub {
    owner = "google";
    repo = "ko";
    rev = "v${version}";
    sha256 = "0fx7yyqajh6hrgrdqlc0z8qcz774i6bql7g8ymav70ja8hv9hvqp";
  };

  meta = with stdenv.lib; {
    homepage = https://github.com/google/ko;
    description = "Build and deploy Go applications on Kubernetes";
    license = licenses.asl20;
    maintainers = with maintainers; [ vdemeester ];
  };
}
