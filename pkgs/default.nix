{ system ? builtins.currentSystem }:
let
  pkgs = import <nixpkgs> { inherit system; };
in
rec {
  scripts = import ./scripts {
    inherit (pkgs) stdenv;
  };
  tmux-tpm = import ./tmux-tpm {
    inherit (pkgs) stdenv lib fetchFromGitHub;
  };

  vrsync = import ./vrsync {
    inherit (pkgs) stdenv lib;
  };
  vde-thinkpad = import ./vde-thinkpad {
    inherit (pkgs) stdenv lib;
  };
}
