{
  pkgs,
  lib,
  ...
}:
{
  imports = [
    ./tiling-common.nix
  ];

  programs.niri = {
    enable = true;
  };
}
