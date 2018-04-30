{ pkgs, prefix, ...}:

{
  programs.autorandr = {
    enable = true;
  };
  services.udiskie = {
    enable = true;
  };
  services.screen-locker = {
    enable = true;
    lockCmd = "i3lock-color --clock -i $HOME/.background-lock --tiling";
    inactiveInterval = 15;
  };
  services.redshift = {
    enable = true;
    brightness = { day = "1"; night = "0.9"; };
    latitude = "48.3";
    longitude = "7.5";
    tray = true;
  };
  xdg.configFile."fish/conf.d/docker.fish".text = ''
    set -gx TESTKIT_AWS_KEYNAME "vdemeester-wakasu"
  '';
  home.packages = with pkgs; [
    slack
    vscode
    spotify
    zoom-us
    youtube-dl
    i3lock-color
  ];
  nixpkgs.config = {
    allowUnfree = true;
  };
}
