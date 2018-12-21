{ system ? builtins.currentSystem }:

let
  pkgs = import <nixpkgs> { inherit system; };
in rec {
  scripts = import ./scripts {
    inherit (pkgs) stdenv;
  };
  tmux-tpm = import ./tmux-tpm {
    inherit (pkgs) stdenv lib fetchFromGitHub;
  };

}
