{ stdenv, lib, fetchurl }:

with lib;
rec {
  ocGen =
    { version
    , sha256
    }:

    stdenv.mkDerivation rec {
      pname = "oc";
      name = "${pname}-${version}";

      src = fetchurl {
        url = "https://mirror.openshift.com/pub/openshift-v4/clients/ocp/${version}/openshift-client-linux-${version}.tar.gz";
        sha256 = "${sha256}";
      };

      phases = " unpackPhase installPhase fixupPhase ";

      unpackPhase = ''
        runHook preUnpack
        mkdir ${name}
        tar -C ${name} -xzf $src
      '';

      installPhase = ''
        runHook preInstall
        install -D ${name}/oc $out/bin/oc
        patchelf \
          --set-interpreter $(cat $NIX_CC/nix-support/dynamic-linker) \
          $out/bin/oc
        # completions
        mkdir -p $out/share/bash-completion/completions/
        $out/bin/oc completion bash > $out/share/bash-completion/completions/oc
        mkdir -p $out/share/zsh/site-functions
        $out/bin/oc completion zsh > $out/share/zsh/site-functions/_oc
      '';
    };

  oc = oc_4_7;
  # oc_4_8 = makeOverridable ocGen {
  #   version = "4.6.6";
  #   sha256 = "10q449fygw5qi9spg1jls6j425fdrrmyvcq6p8z91bwyzn67fds9";
  # };
  oc_4_7 = makeOverridable ocGen {
    version = "4.7.9";
    sha256 = "0ns121x3s8jf2mdswss388c187snlpn0bh7pc34qmx0xb1974vhk";
  };
  oc_4_6 = makeOverridable ocGen {
    version = "4.6.26";
    sha256 = "0jk3ghb3ln88ryxm6ab1dixd7321vjqd77hsp42zf5q7fwzxs6sv";
  };
  oc_4_5 = makeOverridable ocGen {
    version = "4.5.38";
    sha256 = "13lnldf4vv73y4hdzalnp6c16f1mwq662diliq69krlcbjfr6y6x";
  };
  oc_4_4 = makeOverridable ocGen {
    version = "4.4.33";
    sha256 = "1bf34331c7wixb1kmabp93sdpp4ihlyigx7nhmy71ffp7j82n419";
  };
  oc_4_3 = makeOverridable ocGen {
    version = "4.3.40";
    sha256 = "04dkmw78dwniyjdvqyig7hafcicrn2ixg6bvqsy1y2plm0gsjmbz";
  };
  oc_4_2 = makeOverridable ocGen {
    version = "4.2.36";
    sha256 = "1f9h58mx0a3zhpx11gim13hd3m4yzwa6ipbp1gwlghmhjz1jh35v";
  };
  oc_4_1 = makeOverridable ocGen {
    version = "4.1.41";
    sha256 = "06wphg4vddhvavhxn07iq6pi3gq7ljbcdsgldwhyrjy8gx50bp47";
  };
}
