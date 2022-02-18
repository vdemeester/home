{ stdenv, lib, buildGoModule, fetchFromGitHub }:

with lib;
rec {
  tknGen = { version, sha256 }:
    buildGoModule rec {
      pname = "tkn";
      name = "${pname}-${version}";

      subPackages = [ "cmd/tkn" ];
      ldflags = [
        "-s"
        "-w"
        "-X github.com/tektoncd/cli/pkg/cmd/version.clientVersion=${version}"
      ];
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
      meta = with lib; {
        homepage = https://github.com/tektoncd/cli;
        description = "A CLI for interacting with Tekton!";
        license = licenses.asl20;
        maintainers = with maintainers; [ vdemeester ];
      };
    };

  tkn = tkn_0_22;
  tkn_0_22 = makeOverridable tknGen {
    version = "0.22.0";
    sha256 = "sha256-AmJN7hnYuhxYNG/qs7yv3phhffYKVaM8f7irhi9wRfA=";
  };
  tkn_0_21 = makeOverridable tknGen {
    version = "0.21.0";
    sha256 = "166jjf78crwkfzl7vd92y75x85jqlbid55ny3fhl96y456gmhrsl";
  };
  tkn_0_20 = makeOverridable tknGen {
    version = "0.20.0";
    sha256 = "125q6r90q9lz1qn3s3xa2006waxc5yyvgrswzgkczs4bv727am39";
  };
  tkn_0_19 = makeOverridable tknGen {
    version = "0.19.1";
    sha256 = "0vpwbmz8ij6kn61b0r3ayq9vqdsq8yb6scjnd6drb62b9r755qkn";
  };
  tkn_0_18 = makeOverridable tknGen {
    version = "0.18.0";
    sha256 = "1a8a82zy23lc00jnd3cklvrz6si9j84rg2rbq0cm35qladynk75x";
  };
  tkn_0_17 = makeOverridable tknGen {
    version = "0.17.2";
    sha256 = "0wqy8y19l6kay3v2danj10gw58j6l9b3616zn9w5pjbmbcwbslgd";
  };
}
