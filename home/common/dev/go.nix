{ config, pkgs, ... }:
{
  home.sessionVariables = {
    # GOPATH = "${config.home.homeDirectory}";
    GOPATH = "${config.xdg.dataHome}/go";
  };
  home.packages = with pkgs; [
    gcc
    go_1_25
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
    # protobuf
    # ram
    # not really go but still
    # cue
    deptree
  ];
}
