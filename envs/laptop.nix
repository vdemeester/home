{ pkgs, prefix, ...}:

{
  home.packages = with pkgs; [
    autorandr
  ];
}
