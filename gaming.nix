{ pkgs, prefix, ...}:

{
  home.packages = with pkgs; [
    steam
    discord
  ];
}
