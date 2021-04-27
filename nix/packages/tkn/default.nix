{ stdenv, lib, buildGoModule, fetchFromGitHub }:

with lib;
rec {
  tknGen = { version, sha256 }:
    buildGoModule rec {
      pname = "tkn";
      name = "${pname}-${version}";

      subPackages = [ "cmd/tkn" ];
      buildFlagsArray =
        let
          t = "github.com/tektoncd/cli/pkg/cmd/version";
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

  tkn = tkn_0_18;
  tkn_0_18 = makeOverridable tknGen {
    version = "0.18.0";
    sha256 = "1a8a82zy23lc00jnd3cklvrz6si9j84rg2rbq0cm35qladynk75x";
  };
  tkn_0_17 = makeOverridable tknGen {
    version = "0.17.2";
    sha256 = "0wqy8y19l6kay3v2danj10gw58j6l9b3616zn9w5pjbmbcwbslgd";
  };
  tkn_0_16 = makeOverridable tknGen {
    version = "0.16.0";
    sha256 = "0r882qsdz2hhl2fygx48isnak603c2xcngws9145xvxk87gky1ac";
  };
  tkn_0_15 = makeOverridable tknGen {
    version = "0.15.0";
    sha256 = "0xb2zlpkh9cwinp6zj2jpv4wlws042ad1fa0wkcnnkh0vjm6mnrl";
  };
  tkn_0_14 = makeOverridable tknGen {
    version = "0.14.0";
    sha256 = "1mkbwh4cmhx9in928vlvs7xjjklpsxbv5niv8jmsbnifflg1an8p";
  };
  tkn_0_13 = makeOverridable tknGen {
    version = "0.13.1";
    sha256 = "0cjih8h64wwdp022pn70xqxafdk34z2y2ipxb86dlf2zdrf9xv53";
  };
}
