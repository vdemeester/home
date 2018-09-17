{ pkgs, prefix, ... }:

{
  imports = [ ./dev.nix ];
  xdg.configFile."fish/conf.d/go.fish".source = ./fish/go.fish;
  programs.fish.shellAbbrs = {
    got = "go test -v";
    gob = "go build -v";
    gol = "golangci-lint run";
  };
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
    protobuild
  ];
}
