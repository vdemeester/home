{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.dev.go;
in
{
  options = {
    profiles.dev.go = {
      enable = mkOption {
        default = false;
        description = "Enable go development profile";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.sessionVariables = {
        GOPATH = "${config.home.homeDirectory}";
      };
      profiles.dev.enable = true;
      home.packages = with pkgs; [
        gcc
        go
        gocode
        godef
        golangci-lint
        golint
        gomodifytags
        gopkgs
        gotests
        gotools
        go-outline
        go-symbols
        goconvey
        delve
        # vendoring tools
        vndr
        dep
        nur.repos.vdemeester.dep-collector
        # misc
        protobuf
        nur.repos.vdemeester.protobuild
        nur.repos.vdemeester.ram
        nur.repos.vdemeester.sec
        nur.repos.vdemeester.goreturns
        nur.repos.vdemeester.esc
        nur.repos.vdemeester.yaspell
      ];
      xdg.configFile."nr/go" = {
        text = builtins.toJSON [
          {cmd = "pprof"; chan = "unstable";}
          {cmd = "go2nix"; }
          {cmd = "dep2nix"; }
        ];
        onChange = "${pkgs.nur.repos.vdemeester.nr}/bin/nr go";
      };
    }
    (mkIf config.profiles.fish.enable {
      xdg.configFile."fish/conf.d/go.fish".source = ./assets/fish/go.fish;
      programs.fish.shellAbbrs = {
        got = "go test -v";
        gob = "go build -v";
        gol = "golangci-lint run";
      };
    })
  ]);
}
