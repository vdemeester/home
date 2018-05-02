{ pkgs, prefix, ...}:

{
  imports = [
    ./dev.go.nix
    ./dev.rust.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.haskell.nix
  ];
  services.redshift = {
    enable = true;
    brightness = { day = "1"; night = "0.9"; };
    latitude = "48.3";
    longitude = "7.5";
    tray = true;
  };
  services.random-background = {
    enable = true;
    imageDirectory = "/home/vincent/desktop/pictures/wallpapers/Unsplashed";
  };
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx TESTKIT_AWS_KEYNAME "vdemeester-wakasu"
  '';
  home.packages = with pkgs; [
    slack
    vscode
    zoom-us
  ];
}
