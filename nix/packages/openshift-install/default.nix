{ stdenv, lib, fetchurl }:

with lib;
rec {
  openshiftInstallGen =
    { version
    , sha256
    }:

    stdenv.mkDerivation rec {
      pname = "openshift-install";
      name = "${pname}-${version}";

      src = fetchurl {
        inherit sha256;
        url = "https://mirror.openshift.com/pub/openshift-v4/clients/ocp/${version}/openshift-install-linux-${version}.tar.gz";
      };

      phases = " unpackPhase installPhase fixupPhase ";

      unpackPhase = ''
        runHook preUnpack
        mkdir ${name}
        tar -C ${name} -xzf $src
      '';

      installPhase = ''
        runHook preInstall
        install -D ${name}/openshift-install $out/bin/openshift-install
        # completions
        mkdir -p $out/share/bash-completion/completions/
        $out/bin/openshift-install completion bash > $out/share/bash-completion/completions/openshift-install
        #mkdir -p $out/share/zsh/site-functions
        #$out/bin/openshift-install completion zsh > $out/share/zsh/site-functions/_openshift-install
      '';

      meta = {
        description = "Install an OpenShift cluster";
        homepage = https://github.com/openshift/installer;
        license = lib.licenses.asl20;
      };
    };

  openshift-install = openshift-install_4_8;
  openshift-install_4_8 = makeOverridable openshiftInstallGen {
    version = "4.8.2";
    sha256 = "0c6df6iaxlh4blyb6n54qci0m2mx2n24xygqgp313hav4008y5yn";
  };
  openshift-install_4_7 = makeOverridable openshiftInstallGen {
    version = "4.7.21";
    sha256 = "0y38h71n19cav33dbzn7pcgqjqai613f14n53m2lpsm0w3xm58d1";
  };
  openshift-install_4_6 = makeOverridable openshiftInstallGen {
    version = "4.6.40";
    sha256 = "0z92v8yaxnackyisz5czihk8ja0cpbnicfi813b6s9yjlwkbvd19";
  };
  openshift-install_4_5 = makeOverridable openshiftInstallGen {
    version = "4.5.41";
    sha256 = "1356q49bhl0bkqspalh2xymma8rlr5v8xdcfmhflisim930iimw3";
  };
  openshift-install_4_4 = makeOverridable openshiftInstallGen {
    version = "4.4.33";
    sha256 = "1mdjccyz4wbcwvg4ax2wr3x52cx5h8a0i25n1a7aawsni6z5261m";
  };
  openshift-install_4_3 = makeOverridable openshiftInstallGen {
    version = "4.3.40";
    sha256 = "02in21q7d2bazrgkksk0vy2rflxqssayjjlz43k65cnrk8nnv1sm";
  };
}
