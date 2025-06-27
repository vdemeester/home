{ pkgs, ... }:
{
  programs.mpv = {
    enable = true;
    config = {
      hwdec = "auto";
      osc = "no";
    };
    bindings = {
      WHEEL_UP = "seek 10";
      WHEEL_DOWN = "seek -10";
      WHEEL_LEFT = "add volume -2";
      WHEEL_RIGHT = "add volume 2";
      "Alt+0" = "set window-scale 0.5";
    };
    scripts = [
      pkgs.mpvScripts.mpris
      pkgs.mpvScripts.modernz
    ];
  };
}
