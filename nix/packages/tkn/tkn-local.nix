{ stdenv, lib, buildGoModule, fetchFromGitHub }:

with lib;
rec {
  tknLocalGen = { version, sha256 }:
    buildGoModule rec {
      pname = "tkn-local";
      name = "${pname}-${version}";

      subPackages = [ "cmd/tkn-local" ];
      ldflags = [
        "-s"
        "-w"
      ];
      src = fetchFromGitHub {
        owner = "vdemeester";
        repo = "buildkit-tekton";
        rev = "v${version}";
        sha256 = "${sha256}";
      };
      vendorSha256 = null;
      doCheck = false;

      postInstall = ''
        # manpages
        manRoot="$out/share/man"
        mkdir -p "$manRoot/man1"
        for manFile in docs/man/man1/*; do
          manName="$(basename "$manFile")" # "docker-build.1"
          gzip -c "$manFile" > "$manRoot/man1/$manName.gz"
        done
        # completions
        mkdir -p $out/share/bash-completion/completions/
        $out/bin/tkn-local completion bash > $out/share/bash-completion/completions/tkn-local
        mkdir -p $out/share/zsh/site-functions
        $out/bin/tkn-local completion zsh > $out/share/zsh/site-functions/_tkn-local
      '';
      meta = with lib; {
        homepage = https://github.com/vdemeester/buildkit-tekton;
        description = "A Tekton CLI extension for running tekton resources locally";
        license = licenses.asl20;
        maintainers = with maintainers; [ vdemeester ];
      };
    };
  tkn-local = tkn-local_0_3;
  tkn-local_0_3 = makeOverridable tknLocalGen {
    version = "0.3.0";
    sha256 = "sha256-HVz01bOxKgwscwkit3XhpHfSXpMaA7+6CpkgOeQRQY8==";
  };
  tkn-local_0_2 = makeOverridable tknLocalGen {
    version = "0.2.0";
    sha256 = "sha256-Y4wFFdUxzveHguJfs9LQRCqAc143hwNqGx0TJv9cr44=";
  };
}
