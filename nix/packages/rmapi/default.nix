{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "rmapi-${version}";
  version = "0.0.12";
  rev = "v${version}";

  src = fetchFromGitHub {
    inherit rev;
    owner = "juruen";
    repo = "rmapi";
    sha256 = "049m0p4wbsl822iym8xmc938d4k90iw7wjdzcxfyy8d27hv9mp45";
  };
  vendorSha256 = "077s13pcql5w2m6wzls1q06r7p501kazbwzxgfh6akwza15kb4is";

  meta = {
    description = "Go app that allows you to access your reMarkable tablet files through the Cloud API";
    homepage = "https://github.com/juruen/rmapi";
    license = lib.licenses.gpl3;
  };
}
