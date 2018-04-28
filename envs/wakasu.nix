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
  programs.fish = {
    enable = true;
    shellAbbrs = {
      gs = "git status";
    };
  };
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
