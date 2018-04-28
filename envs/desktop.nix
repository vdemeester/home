{ pkgs, prefix, ...}:

{
  programs.firefox = {
    enable = true;
  };
  programs.termite = {
    enable = true;
    font = "Ubuntu Mono 16";
    sizeHints = true;
  };
  home.packages = with pkgs; [
    xdg-user-dirs
    xdg_utils
  ];
}
