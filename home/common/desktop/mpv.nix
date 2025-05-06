{ pkgs, ... }:
{
  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
      osc = "no";
    };
    scripts = [
      pkgs.mpvScripts.mpris
      pkgs.mpvScripts.modernz
    ];
  };
}
