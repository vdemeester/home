{ pkgs, ... }:
{
  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
    };
    scripts = [ pkgs.mpvScripts.mpris pkgs.mpvScripts.modernz ];
  };
}
