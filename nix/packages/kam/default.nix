{ stdenv, lib, fetchurl }:

with lib;
rec {
  kamGen =
    { version
    , sha256
    }:

    stdenv.mkDerivation rec {
      pname = "kam";
      name = "${pname}-${version}";

      src = fetchurl {
        url = "https://mirror.openshift.com/pub/openshift-v4/clients/kam/v${version}/kam-linux-amd64.tar.gz";
        sha256 = "${sha256}";
      };

      phases = " unpackPhase installPhase fixupPhase ";

      unpackPhase = ''
        runHook preUnpack
        mkdir ${name}
        tar -C ${name} -xzf $src
      '';

      installPhase = ''
        runHook preInstall
        install -D ${name}/kam $out/bin/kam
        patchelf \
          --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
          $out/bin/kam
        # completions
        mkdir -p $out/share/bash-completion/completions/
        $out/bin/kam completion bash > $out/share/bash-completion/completions/kam
        mkdir -p $out/share/zsh/site-functions
        $out/bin/kam completion zsh > $out/share/zsh/site-functions/_kam
      '';
    };

  kam = kam_1_1;
  kam_1_1 = makeOverridable kamGen {
    version = "1.1.1";
    sha256 = "0cxf2n6y9arabq4ri3ni8v17vaflg9xqycmjwv2cm13bj4d7a1nq";
  };
}
