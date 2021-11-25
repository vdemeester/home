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

  oc = oc_4_9;
  oc_4_9 = makeOverridable ocGen {
    version = "4.9.9";
    sha256 = "sha256-2n99D2PFEHBV7Cd6Co9S/QXGcMTDsWwFSNVG9ggvEBY=";
  };
  oc_4_8 = makeOverridable ocGen {
    version = "4.8.22";
    sha256 = "sha256-sGk7LPDhe3wjG4U2Covu9az2CVROCKRqoB1YxP73/vk=";
  };
  oc_4_7 = makeOverridable ocGen {
    version = "4.7.38";
    sha256 = "sha256-HC/KRRpz2QpdjHpOyoYolZ6CAKBOFyq5Yh3YYszjNFM=";
  };
  oc_4_6 = makeOverridable ocGen {
    version = "4.6.50";
    sha256 = "sha256-tkbLQHzT2AnkYqpfwGH30uFIuijwlrHsBCiKLGA0gMI=";
  };
  oc_4_5 = makeOverridable ocGen {
    version = "4.5.41";
    sha256 = "01m7iak83md7y1nfdywizr5sxk3kd8543ahzqh2ncj9sqmjraw0r";
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
