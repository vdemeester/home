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
    version = "4.7.9";
    sha256 = "0q27wgak4a8qp7bckssz555mrnxrj5ngf2561h6nxnxb194q45m4";
  };
  openshift-install_4_6 = makeOverridable openshiftInstallGen {
    version = "4.6.26";
    sha256 = "1l65ggfz249885cj63i415j51n8pmvlc65xnjfcacdyiv9s17915";
  };
  openshift-install_4_5 = makeOverridable openshiftInstallGen {
    version = "4.5.38";
    sha256 = "03nylxdk4iihf246ihvszdbbxp2dbdv7yjp0jrkq42pfb638qzv1";
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
