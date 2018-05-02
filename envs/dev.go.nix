{ pkgs, prefix, ... }:

{
  imports = [ ./dev.nix ];
  home.packages = with pkgs; [
    go
    gcc
    # tools
    gotools
    golint
    godef
    gocode
    gotests
    gopkgs
    gomodifytags
    go-outline
    go-symbols
    goconvey
    delve
    # vendoring tools
    vndr
    dep
    # misc
    protobuf
  ];
}
