{ stdenv, lib, buildGoModule, git, fetchFromGitHub }:

with lib;
rec {
  operatorSdkGen =
    { version
    , sha256
    , vendorSha256
    }:

    buildGoModule rec {
      inherit vendorSha256;
      pname = "operator-sdk";
      name = "${pname}-${version}";
      rev = "v${version}";

      builtInputs = [ "git" ];

      subPackages = [ "cmd/operator-sdk" ];
      buildFlagsArray =
        let
          t = "github.com/operator-framework/operator-sdk/version";
        in
        ''
          -ldflags=
            -X ${t}.GitVersion=${version}
            -X ${t}.KubernetesVersion=v1.17.2
        '';

      src = fetchFromGitHub {
        inherit rev;
        owner = "operator-framework";
        repo = "operator-sdk";
        sha256 = "${sha256}";
      };
      modSha256 = "${vendorSha256}";

      postInstall = ''
        # completions
        mkdir -p $out/share/bash-completion/completions/
        $out/bin/operator-sdk completion bash > $out/share/bash-completion/completions/operator-sdk
        mkdir -p $out/share/zsh/site-functions/
        $out/bin/operator-sdk completion zsh > $out/share/zsh/site-functions/_operator-sdk
      '';

      meta = {
        description = "SDK for building Kubernetes applications. Provides high level APIs, useful abstractions, and project scaffolding";
        homepage = https://github.com/operator-framework/operator-sdk;
        license = lib.licenses.asl20;
      };
    };

  operator-sdk_0_18 = makeOverridable operatorSdkGen {
    version = "0.18.0";
    sha256 = "1jbi5v1kcni740znjxm6hbpjx3a1zlkgnbnpsqbiljfi6k7spn6p";
    vendorSha256 = "08n6r0d7gqiysyl348l698blr7y9cxdmcadbmymzcya0fmczp0mv";
  };
  operator-sdk_0_19 = makeOverridable operatorSdkGen {
    version = "0.19.0";
    sha256 = "0prwdsxm8sldzdn7r9k1yw2q4asz90n25g4zcfaq23vs6fkxch9b";
    vendorSha256 = "0k6xrd1ahic89l821qvh3rr8k203ab6fmj7v4flkv889xaajjxb0";
  };
  operator-sdk_1_13 = makeOverridable operatorSdkGen {
    version = "1.13.0";
    sha256 = "107rkh11qql77i0ahgipfgih843zfrbvf1k7gfrmvp2avns98m3x";
    vendorSha256 = "1mvwrnik3mapghvi41zdxfd6d2d08hafnghypqqjwgdnin06bg98";
  };
  operator-sdk_1 = operator-sdk_1_13;
  operator-sdk = operator-sdk_1;
}
