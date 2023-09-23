{ stdenv, lib, buildFHSUserEnv }:

buildFHSUserEnv {
  name = "fhs-std";
  targetPkgs = pkgs: with pkgs; [
    envsubst
    eza
    git
    gnumake
    coreutils
    zsh
  ];
  runScript = "/bin/zsh";
}
