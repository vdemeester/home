{ pkgs, prefix, ...}:

{
  home.packages = [
    pkgs.jq
    pkgs.htop
    pkgs.pass
  ];
}
