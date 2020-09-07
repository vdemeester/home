{ stdenv, lib, buildGoModule, fetchFromGitHub }:

with lib;
rec {
  tknGen = { version, sha256, modSha }:
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
      modSha256 = "${modSha}";
      vendorSha256 = "${modSha}";

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

  tkn_0_12 = makeOverridable tknGen {
    version = "0.12.0";
    sha256 = "08mw8g31f4v2n55hsb5106r5ng9lklx66xfx0v580m7fbrdb83gs";
    modSha = "1475423l1hgx51zxd5k78j05198bblf6cmqjma4wnpwl4z2qcq57";
  };
  tkn_0_11 = makeOverridable tknGen {
    version = "0.11.0";
    sha256 = "19svynznk7bshjm9hd0zxzdn5j09fl7n7jws2hf8qm1y0ynbydmb";
    modSha = "0cs8lh3n84qxwybd9jf2wx3pj8drpj2bl9p31p5v4bs9k26m2jw4";
  };
  tkn_0_10 = makeOverridable tknGen {
    version = "0.10.0";
    sha256 = "1jdhd11g7kvq2c0cda888gvdpqhzi8srf4si2v951madr7dai2cl";
    modSha = "1cdhs794habhhv1242ffv3lpkddx6rk2wjiyknf3lq6q47xlzz24";
  };
  tkn_0_9 = makeOverridable tknGen {
    version = "0.9.0";
    sha256 = "1x4fk10dwrv3sm8gli7z248jzjjvs8j49rqs7s6gqby8pck5ihq7";
    modSha = "160174vw34v9w53azkzslcskzhsk1dflccfbvl1l38xm624fj4lw";
  };
}
