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

  openshift-install = openshift-install_4_11;
  openshift-install_4_11 = makeOverridable openshiftInstallGen {
    version = "4.11.2";
    sha256 = "sha256-WbV8O4ovOvCVyplDZHkLPM4vh9fH2e0dgIIJzOCDTt4=";
  };
  openshift-install_4_10 = makeOverridable openshiftInstallGen {
    version = "4.10.30";
    sha256 = "sha256-EzgFJAf91I+SjyIKdhlHQR7ET22xi/wzHRFCau5Y+TY=";
  };
  openshift-install_4_9 = makeOverridable openshiftInstallGen {
    version = "4.9.47";
    sha256 = "sha256-dgxl1syjyzcY6Z/W0qzHe3/GIj4niD+Epx0DFofw7tY=";
  };
  openshift-install_4_8 = makeOverridable openshiftInstallGen {
    version = "4.8.48";
    sha256 = "sha256-Vsoa5mg21epGQRKvOEA/qIyVNqc9QPMmKlY9DZ5J2wQ=";
  };
  openshift-install_4_7 = makeOverridable openshiftInstallGen {
    version = "4.7.56";
    sha256 = "sha256-f0SmZewcHIqrQr5IZ4oxhT5LfcyHMejA/wI+AoI09d0=";
  };
  openshift-install_4_6 = makeOverridable openshiftInstallGen {
    version = "4.6.50";
    sha256 = "sha256-NM3FIe6d2iPlRSZElklhD7dF2LnxYdzEkTCNpTaCHnQ=";
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
