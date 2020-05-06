{ pkgs ? import <nixpkgs> {} }:

rec {
  # pre nur-packages import
  scripts = pkgs.callPackage ./scripts {};
  tmux-tpm = pkgs.callPackage ./tmux-tpm {};
  vrsync = pkgs.callPackage ./vrsync {};
  vde-thinkpad = pkgs.callPackage ./vde-thinkpad {};

  # Mine
  ape = pkgs.callPackage ./ape {};
  fhs-std = pkgs.callPackage ./fhs/std.nix {};
  nr = pkgs.callPackage ./nr {};
  ram = pkgs.callPackage ./ram {};
  sec = pkgs.callPackage ./sec {};
  systemd-email = pkgs.callPackage ./systemd-email {};
  yak = pkgs.callPackage ./yak {};

  # Maybe upstream
  athens = pkgs.callPackage ./athens {};
  dobi = pkgs.callPackage ./dobi {};
  dep-collector = pkgs.callPackage ./dep-collector {};
  envbox = pkgs.callPackage ./envbox {};
  esc = pkgs.callPackage ./esc {};
  gogo-protobuf = pkgs.callPackage ./gogo-protobuf {};
  goreturns = pkgs.callPackage ./goreturns {};
  gorun = pkgs.callPackage ./gorun {};
  govanityurl = pkgs.callPackage ./govanityurl {};
  knctl = pkgs.callPackage ./knctl {};
  ko = pkgs.callPackage ./ko {};
  kss = pkgs.callPackage ./kss {};
  kubernix = pkgs.callPackage ./kubernix {};
  krew = pkgs.callPackage ./krew {};
  prm = pkgs.callPackage ./prm {};
  protobuild = pkgs.callPackage ./protobuild {};
  rmapi = pkgs.callPackage ./rmapi {};
  toolbox = pkgs.callPackage ./toolbox {};
  yaspell = pkgs.callPackage ./yaspell {};

  # OpenShift
  inherit (pkgs.callPackage ./oc {})
    oc_4_1
    oc_4_2
    oc_4_3
    oc_4_4
    ;
  oc = oc_4_4;
  openshift-install = pkgs.callPackage ./openshift-install {};

  inherit (pkgs.callPackage ./crc {})
    crc_1_9
    crc_1_10
    ;
  crc = crc_1_10;

  # Operator SDK
  operator-sdk = pkgs.callPackage ./operator-sdk {};

  # Tekton
  inherit (pkgs.callPackage ./tkn {})
    tkn_0_8
    tkn_0_9
    ;
  tkn = tkn_0_9;

  # Upstream
  buildkit = pkgs.callPackage ./buildkit {};
  inherit (pkgs.callPackage ./containerd {})
    containerd_1_2
    containerd_1_3
    ;
  containerd = containerd_1_3;
}
