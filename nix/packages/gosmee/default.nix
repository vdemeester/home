{ stdenv, lib, buildGoModule, fetchFromGitHub }:

buildGoModule rec {
  name = "gosmee-${version}";
  version = "0.14.0";
  rev = "${version}";

  src = fetchFromGitHub {
    inherit rev;
    owner = "chmouel";
    repo = "gosmee";
    sha256 = "sha256-VKH0ajtLndTdW0dOY3XpixkTGy7Kvac7poFZVzj5HQU=";
  };
  vendorHash = null;

  postUnpack = ''
    printf ${version} > $sourceRoot/gosmee/templates/version
  '';

  postInstall = ''
    # completions
    mkdir -p $out/share/bash-completion/completions/
    $out/bin/gosmee completion bash > $out/share/bash-completion/completions/gosmee
    mkdir -p $out/share/zsh/site-functions
    $out/bin/gosmee completion zsh > $out/share/zsh/site-functions/_gosmee
  '';

  meta = {
    description = "Command line server and client for webhooks deliveries (and https://smee.io)";
    homepage = "https://github.com/chmouel/gosmee";
    license = lib.licenses.asl20;
  };
}
