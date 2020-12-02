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
        #mkdir -p $out/share/bash-completion/completions/
        #$out/bin/openshift-install completion bash > $out/share/bash-completion/completions/openshift-install
        #mkdir -p $out/share/zsh/site-functions
        #$out/bin/openshift-install completion zsh > $out/share/zsh/site-functions/_openshift-install
      '';

      meta = {
        description = "Install an OpenShift cluster";
        homepage = https://github.com/openshift/installer;
        license = lib.licenses.asl20;
      };
    };

  openshift-install = openshift-install_4_6;
  openshift-install_4_6 = makeOverridable openshiftInstallGen {
    version = "4.6.6";
    sha256 = "0r4cj8iz76cy3zv80j8l0lfmimqk80lfnqxm412dh9qmnc5xsl62";
  };
  openshift-install_4_5 = makeOverridable openshiftInstallGen {
    version = "4.5.22";
    sha256 = "1na3kp7v6gqk2wr1w5gpr9dy9nr1vw4z3kka776sxmc082bamg1f";
  };
  openshift-install_4_4 = makeOverridable openshiftInstallGen {
    version = "4.4.31";
    sha256 = "11iykrkgx3f085vcnk5gl9ia2263ryffpvrz3rj1zgpgpdir6m45";
  };
  openshift-install_4_3 = makeOverridable openshiftInstallGen {
    version = "4.3.40";
    sha256 = "02in21q7d2bazrgkksk0vy2rflxqssayjjlz43k65cnrk8nnv1sm";
  };
}
