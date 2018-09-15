self: super: {
  ape = import ../pkgs/ape {
    inherit (self) stdenv lib buildGoPackage fetchFromGitHub;
  };
  vrsync = import ../pkgs/vrsync {
    inherit (self) stdenv lib;
  };
  runc-edge = import ../pkgs/runc {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo go-md2man go pkgconfig libapparmor apparmor-parser libseccomp;
  };
  containerd-edge = import ../pkgs/containerd {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo go btrfs-progs;
  };
  cni = import ../pkgs/cni {
    inherit (self) stdenv fetchFromGitHub go;
  };
  cni-plugins = import ../pkgs/cni/plugins.nix {
    inherit (self) stdenv lib fetchFromGitHub go;
  };
}
