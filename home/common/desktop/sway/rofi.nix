{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    raffi
  ];

  programs.fuzzel = {
    enable = true;
    settings = {
      main = {
        font = "JetBrains Mono:size=12";
        dpi-aware = "auto";
      };
      colors = {
        background = "1f1e25ff";
        text = "969696ff";
        match = "00d3d0ff";
        selection = "303030ff";
        selection-match = "ffffffff";
        selection-text = "ffffffff";
        border = "348e9eff";
      };
      border = {
        width = 2;
        radius = 2;
      };
      key-bindings = {
        cancel = "Escape Control+g";
      };
    };
  };

  # FIXME updates this
  xdg.configFile."raffi/raffi.yaml".text = lib.generators.toYAML { } {
    "firefox" = {
      binary = "firefox";
      args = [ "--marionette" ];
    };
  };
}
