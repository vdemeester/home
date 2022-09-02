{ stdenv, lib, buildGo117Module, git, fetchFromGitHub }:

with lib;
rec {
  opmGen =
    { version
    , sha256
    , vendorSha256
    }:

    buildGo117Module rec {
      inherit vendorSha256;
      pname = "opm";
      name = "${pname}-${version}";
      rev = "v${version}";

      builtInputs = [ "git" ];

      subPackages = [ "cmd/opm" ];
      ldflags =
        let
          t = "https://github.com/operator-framework/operator-registry/cmd/opm/version";
        in
        [
          "-X ${t}.gitCommit=${version}"
          "-X ${t}.opmVersion=${version}"
        ];

      src = fetchFromGitHub {
        inherit rev;
        owner = "operator-framework";
        repo = "operator-registry";
        sha256 = "${sha256}";
      };

      postInstall = ''
        # completions
        mkdir -p $out/share/bash-completion/completions/
        $out/bin/opm completion bash > $out/share/bash-completion/completions/opm
        mkdir -p $out/share/zsh/site-functions/
        $out/bin/opm completion zsh > $out/share/zsh/site-functions/_opm
      '';

      meta = {
        description = "Operator Registry runs in a Kubernetes or OpenShift cluster to provide operator catalog data to Operator Lifecycle Manager.

";
        homepage = https://github.com/operator-framework/operator-registry;
        license = lib.licenses.asl20;
      };
    };

  opm_1_26 = makeOverridable opmGen {
    version = "1.26.0";
    sha256 = "sha256-wOdHduHcuH5olPGwpV6v4wSTxZYZbkzwDGknQVGPQvI=";
    vendorSha256 = null;
  };
  opm = opm_1_26;
}
