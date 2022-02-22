{ config, pkgs, ... }:

{
  home.sessionVariables = {
    GOPATH = "${config.home.homeDirectory}";
  };
  home.packages = with pkgs; [
    gcc
    go_1_17
    godef
    golangci-lint
    golint
    gopkgs
    go-outline
    go-symbols
    delve
    goimports
    gotestsum
    # misc
    protobuf
    my.ram
    my.esc
    my.yaspell
  ];
}
