{ pkgs, prefix, ...}:

{
  home.packages = with pkgs; [
    jq
    htop
    pass
    tree
  ];
}
