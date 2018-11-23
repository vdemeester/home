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
      profiles.dev.enable = true;
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
        nur.repos.vdemeester.dep-collector
        # misc
        protobuf
        nur.repos.vdemeester.protobuild
      ];
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
