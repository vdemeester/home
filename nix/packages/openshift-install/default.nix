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

  openshift-install = openshift-install_4_7;
  openshift-install_4_7 = makeOverridable openshiftInstallGen {
    version = "4.7.4";
    sha256 = "1mnr9llmn8035qnzc6cwpva3f1d1vsw0bxqqn9yvifi1isfl3g2h";
  };
  openshift-install_4_6 = makeOverridable openshiftInstallGen {
    version = "4.6.23";
    sha256 = "1m5ryp1i2infln1ig3hc91qrk60syc8jw2vwpq8f7l9ndg7l9lz6";
  };
  openshift-install_4_5 = makeOverridable openshiftInstallGen {
    version = "4.5.36";
    sha256 = "1s4pc0rfw45insvhv61r5kjjdy5874ivcsm37nmr90nkmpq30lsq";
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
