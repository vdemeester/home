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
  cni = import ../pkgs/cni {
    inherit (self) stdenv fetchFromGitHub go;
  };
  cni-plugins = import ../pkgs/cni/plugins.nix {
    inherit (self) stdenv lib fetchFromGitHub go;
  };
  stellar = import ../pkgs/stellar {
    inherit (self) stdenv lib fetchFromGitHub removeReferencesTo go;
  };
}
