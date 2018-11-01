{ pkgs, prefix, ... }:

{
  profiles.dev.enable = true;
  home.packages = with pkgs; [
    jdk
    gradle
  ];
}
