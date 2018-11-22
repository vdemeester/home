self: super: {
  ape = import ../pkgs/ape {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  vrsync = import ../pkgs/vrsync {
    inherit (self) stdenv lib;
  };
  vde-thinkpad = import ../pkgs/vde-thinkpad {
    inherit (self) stdenv lib;
  };
  runc-edge = import ../pkgs/runc {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo go-md2man go pkgconfig libapparmor apparmor-parser libseccomp;
  };
  containerd-edge = import ../pkgs/containerd {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo go btrfs-progs;
  };
  buildkit = import ../pkgs/buildkit {
    inherit (self) stdenv lib fetchFromGitHub buildGoPackage;
  };
  cni = import ../pkgs/cni {
    inherit (self) stdenv fetchFromGitHub go;
  };
  cni-plugins = import ../pkgs/cni/plugins.nix {
    inherit (self) stdenv lib fetchFromGitHub go;
  };
  stellar = import ../pkgs/stellar {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo go;
  };
  podman = import ../pkgs/podman {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo pkgconfig makeWrapper go libtool gpgme lvm2 btrfs-progs libseccomp gcc;
  };
  conmon = import ../pkgs/conmon {
    inherit (self) stdenv lib fetchFromGitHub makeWrapper pkgconfig libtool gcc glib;
  };
}
