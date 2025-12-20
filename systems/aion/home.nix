{ ... }:
{
  imports = [
    (import ../../home/common/services/beets.nix { baseDir = "/neo/music"; })
  ];
}
