{ pkgs, ... }:

{
  home.packages = with pkgs; [
    roswell
    sbcl
  ];
}
