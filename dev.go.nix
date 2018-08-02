{ pkgs, prefix, ... }:

{
  imports = [ ./dev.nix ];
  xdg.configFile."fish/conf.d/go.fish".text = ''
    set -gx GOPATH $HOME
  '';
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
