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
  xdg.configFile."nr/dev.go" = {
    text = builtins.toJSON [
      { cmd = "pprof"; chan = "unstable"; }
      { cmd = "vndr"; chan = "unstable"; }
      { cmd = "go2nix"; }
      { cmd = "dep2nix"; }
    ];
    onChange = "${pkgs.my.nr}/bin/nr dev.go";
  };
}
