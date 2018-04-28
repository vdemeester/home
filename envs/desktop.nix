{ pkgs, prefix, ...}:

{
  programs.firefox = {
    enable = true;
  };
  home.packages = with pkgs; [
    xdg-user-dirs
    xdg_utils
  ];
}
