{ pkgs, prefix, ... }:

{
  imports = [ ./dev.nix ];
  home.packages = with pkgs; [
    jdk
    gradle
  ];
}
