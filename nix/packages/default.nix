{ pkgs ? import <nixpkgs> { } }:

rec {
  # pre nur-packages import
  scripts = pkgs.callPackage ./my/scripts { };
  vrsync = pkgs.callPackage ./my/vrsync { };
  vde-thinkpad = pkgs.callPackage ./my/vde-thinkpad { };
  bus = pkgs.callPackage ../../tools/bus { };
  bekind = pkgs.callPackage ../../tools/bekind { };

  chmouzies.kubernetes = pkgs.callPackage ./chmouzies/kubernetes.nix { };

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
  kss = pkgs.callPackage ./kss { };
  batzconverter = pkgs.callPackage ./batzconverter { };
  #kubernix = pkgs.callPackage ./kubernix { };
  krew = pkgs.callPackage ./krew { };
  kail = pkgs.callPackage ./kail { };
  prm = pkgs.callPackage ./prm { };
  #protobuild = pkgs.callPackage ./protobuild { };
  rmapi = pkgs.callPackage ./rmapi { };
  toolbox = pkgs.callPackage ./toolbox { };
  yaspell = pkgs.callPackage ./yaspell { };
  gosmee = pkgs.callPackage ./gosmee {
    buildGoModule = pkgs.buildGo120Module;
  };

  inherit (pkgs.callPackage ./kam { })
    kam_1_1
    kam
    ;

  operator-tool = pkgs.callPackage ./operator-tooling { };

  manifest-tool = pkgs.callPackage ./manifest-tool { };

  # Upstream
  buildkit = pkgs.callPackage ./buildkit { };
  buildx = pkgs.callPackage ./buildx { };

  adi1090x-plymouth = pkgs.callPackage ./adi1090x-plymouth { };
}
