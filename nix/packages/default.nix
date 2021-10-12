{ sources ? import ../.
, pkgs ? sources.pkgs { }
}:

rec {
  # pre nur-packages import
  scripts = pkgs.callPackage ./my/scripts { };
  vrsync = pkgs.callPackage ./my/vrsync { };
  vde-thinkpad = pkgs.callPackage ./my/vde-thinkpad { };
  bus = pkgs.callPackage ../../tools/bus { };
  bekind = pkgs.callPackage ../../tools/bekind { };

  # Mine
  ape = pkgs.callPackage ./ape { };
  fhs-std = pkgs.callPackage ./fhs/std.nix { };
  nr = pkgs.callPackage ./nr { };
  ram = pkgs.callPackage ./ram { };
  #sec = pkgs.callPackage ./sec { };
  systemd-email = pkgs.callPackage ./systemd-email { };
  yak = pkgs.callPackage ./yak { };

  # Maybe upstream
  athens = pkgs.callPackage ./athens { };
  envbox = pkgs.callPackage ./envbox { };
  esc = pkgs.callPackage ./esc { };
  #gogo-protobuf = pkgs.callPackage ./gogo-protobuf {};
  gorun = pkgs.callPackage ./gorun { };
  govanityurl = pkgs.callPackage ./govanityurl { };
  ko = pkgs.callPackage ./ko { };
  kss = pkgs.callPackage ./kss { };
  batzconverter = pkgs.callPackage ./batzconverter { };
  #kubernix = pkgs.callPackage ./kubernix { };
  krew = pkgs.callPackage ./krew { };
  prm = pkgs.callPackage ./prm { };
  #protobuild = pkgs.callPackage ./protobuild { };
  rmapi = pkgs.callPackage ./rmapi { };
  toolbox = pkgs.callPackage ./toolbox { };
  yaspell = pkgs.callPackage ./yaspell { };

  # OpenShift
  inherit (pkgs.callPackage ./openshift/oc.nix { })
    oc_4_1
    oc_4_2
    oc_4_3
    oc_4_4
    oc_4_5
    oc_4_6
    oc_4_7
    oc_4_8
    oc
    ;
  inherit (pkgs.callPackage ./openshift/openshift-install.nix { })
    openshift-install_4_3
    openshift-install_4_4
    openshift-install_4_5
    openshift-install_4_6
    openshift-install_4_7
    openshift-install_4_8
    openshift-install
    ;

  inherit (pkgs.callPackage ./openshift/odo.nix { })
    odo_1_2
    odo_2_0
    odo_2_1
    odo_2_2
    odo_2_3
    odo
    ;

  inherit (pkgs.callPackage ./kam { })
    kam_1_1
    kam
    ;

  inherit (pkgs.callPackage ./crc { oc_4_4 = oc_4_4; oc_4_3 = oc_4_3; })
    crc_1_9
    crc_1_10
    crc_1_11
    crc_driver_libvirt_0_12_7
    crc_driver_libvirt_0_12_8
    ;
  crc = crc_1_10;

  # Operator SDK
  inherit (pkgs.callPackage ./operator-sdk { })
    operator-sdk_1
    operator-sdk_1_13
    operator-sdk_0_18
    operator-sdk_0_19
    operator-sdk
    ;

  # Tekton
  inherit (pkgs.callPackage ./tkn { })
    tkn_0_17
    tkn_0_18
    tkn_0_19
    tkn_0_20
    tkn_0_21
    tkn
    ;
  manifest-tool = pkgs.callPackage ./manifest-tool { };

  # Upstream
  buildkit = pkgs.callPackage ./buildkit { };
  buildx = pkgs.callPackage ./buildx { };
  inherit (pkgs.callPackage ./containerd { })
    containerd_1_2
    containerd_1_3
    containerd_1_4
    ;
  containerd = containerd_1_3;

  gnome-shell-extension-shell = pkgs.callPackage ./gnome/extensions/shell { };
  gnome-bluetooth-quick-connect = pkgs.callPackage ./gnome/extensions/bluetooth-quick-connect { };
  hidetopbar = pkgs.callPackage ./gnome/extensions/hide-top-bar { };
  noannoyance = pkgs.callPackage ./gnome/extensions/noannoyance { };
  nightthemeswitcher = pkgs.callPackage ./gnome/extensions/nightthemeswitcher { };
  adi1090x-plymouth = pkgs.callPackage ./adi1090x-plymouth { };
}
