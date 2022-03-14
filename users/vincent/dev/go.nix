{ config, pkgs, ... }:

{
  home.sessionVariables = {
    GOPATH = "${config.home.homeDirectory}";
  };
  home.packages = with pkgs; [
    gcc
    go_1_17
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
    # misc
    protobuf
    my.ram
    my.esc
    my.yaspell
    # not really go but still
    cue
  ];
}
