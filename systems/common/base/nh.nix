{ pkgs, ... }:
{
  programs.nh = {
    enable = true;
    package = pkgs.nh;
    flake = "/home/vincent/src/home";
    clean = {
      enable = true;
      extraArgs = "--keep-since 15d --keep 3";
    };
  };
}
