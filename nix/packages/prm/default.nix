{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "prm-${version}";
  version = "3.4.0";
  rev = "v${version}";

  goPackagePath = "github.com/ldez/prm/v3";
  buildFlagsArray = let t = "${goPackagePath}/meta"; in
    ''
      -ldflags=
         -X ${t}.Version=${version}
         -X ${t}.BuildDate=unknown
    '';

  src = fetchFromGitHub {
    inherit rev;
    owner = "ldez";
    repo = "prm";
    sha256 = "1vpii7046rq13ahjkbk7rmbqskk6x1mcsrzqx91nii7nzl32wdap";
  };
  vendorSha256 = "0hiz514xklhk4c5c7lmx02l04dynnlmjy6mjwx3f7ynxiyk3scgz";

  meta = {
    description = "Pull Request Manager for Maintainers";
    homepage = "https://github.com/ldez/prm";
    license = lib.licenses.asl20;
  };
}
