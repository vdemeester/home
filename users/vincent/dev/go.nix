{ config, pkgs, ... }:

{
  home.sessionVariables = {
    # GOPATH = "${config.home.homeDirectory}";
  };
  home.packages = with pkgs; [
    gcc
    go_1_22
    gopls
    godef
    golangci-lint
    golint
    gopkgs
    go-outline
    go-symbols
    delve
    gotools
    gotestsum
    gofumpt
    # misc
    protobuf
    my.ram
    my.yaspell
    # not really go but still
    gosmee
    # cue
    deptree
  ];
}
