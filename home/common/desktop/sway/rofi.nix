{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    fuzzel
    raffi
  ];

  # FIXME updates this
  xdg.configFile."raffi/raffi.yaml".text = lib.generators.toYAML { } {
    "firefox" = {
      binary = "firefox";
      args = [ "--marionette" ];
    };
  };
}
