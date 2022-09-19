{ pkgs, ... }:

let
  my-spotify = pkgs.writeShellScriptBin "spotify" ''
    exec ${pkgs.spotify}/bin/spotify --enable-features=UseOzonePlatform --ozone-platform=wayland
  '';
in
{
  home.packages = [
    my-spotify
  ];
}
