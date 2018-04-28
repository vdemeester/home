{ pkgs, prefix, ...}:

{
  programs.autorandr = {
    enable = true;
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
  ];
  nixpkgs.config = {
    allowUnfree = true;
  };
}
