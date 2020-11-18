{ stdenv, lib, buildGoModule, fetchFromGitHub }:

with lib;
rec {
  tknGen = { version, sha256 }:
    buildGoModule rec {
      pname = "tkn";
      name = "${pname}-${version}";

      goPackagePath = "github.com/tektoncd/cli";
      subPackages = [ "cmd/tkn" ];
      buildFlagsArray =
        let
          t = "${goPackagePath}/pkg/cmd/version";
        in
        ''
          -ldflags=
            -X ${t}.clientVersion=${version}
        '';
      src = fetchFromGitHub {
        owner = "tektoncd";
        repo = "cli";
        rev = "v${version}";
        sha256 = "${sha256}";
      };
      vendorSha256 = null;

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
        $out/bin/tkn completion bash > $out/share/bash-completion/completions/tkn
        mkdir -p $out/share/zsh/site-functions
        $out/bin/tkn completion zsh > $out/share/zsh/site-functions/_tkn
      '';
      meta = with stdenv.lib; {
        homepage = https://github.com/tektoncd/cli;
        description = "A CLI for interacting with Tekton!";
        license = licenses.asl20;
        maintainers = with maintainers; [ vdemeester ];
      };
    };

  tkn_0_14 = makeOverridable tknGen {
    version = "0.14.0";
    sha256 = "1mkbwh4cmhx9in928vlvs7xjjklpsxbv5niv8jmsbnifflg1an8p";
  };
  tkn_0_13 = makeOverridable tknGen {
    version = "0.13.1";
    sha256 = "0cjih8h64wwdp022pn70xqxafdk34z2y2ipxb86dlf2zdrf9xv53";
  };
  tkn_0_12 = makeOverridable tknGen {
    version = "0.12.0";
    sha256 = "08mw8g31f4v2n55hsb5106r5ng9lklx66xfx0v580m7fbrdb83gs";
  };
  tkn_0_11 = makeOverridable tknGen {
    version = "0.11.0";
    sha256 = "19svynznk7bshjm9hd0zxzdn5j09fl7n7jws2hf8qm1y0ynbydmb";
  };
}
