{ pkgs, prefix, ...}:

{
  imports = [
    ./devops.nix
    ./dev.go.nix
    ./dev.python.nix
    ./dev.js.nix
    ./dev.java.nix
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
    set -gx DOCKER_BUILDKIT 1
  '';
  home.packages = with pkgs; [
    vscode
    zoom-us
    debootstrap
    weechat weechat-xmpp
  ];
}
