{ pkgs, prefix, ... }:

{
  imports = [ ./fish.nix ./ssh.nix ./i3.nix ./base.nix ];
  home.keyboard = {
    layout = "fr(bepo),fr";
    variant = "oss";
    options = ["grp:menu_toggle" "grp_led:caps" "compose:caps"];
  };
  xsession = {
    enable = true;
    initExtra = ''
      ${pkgs.xlibs.xmodmap}/bin/xmodmap ~/.Xmodmap &
    '';
    pointerCursor = {
      package = pkgs.vanilla-dmz;
      name = "Vanilla-DMZ";
    };
  };
  home.file.".XCompose".source = ./xorg/XCompose;
  home.file.".Xmodmap".source = ./xorg/Xmodmap;
  xdg.configFile."xorg/emoji.compose".source = ./xorg/emoji.compose;
  xdg.configFile."xorg/parens.compose".source = ./xorg/parens.compose;
  xdg.configFile."xorg/modletters.compose".source = ./xorg/modletters.compose;
  services.gpg-agent = {
    enable = true;
    enableSshSupport = true;
  };
  services.dunst.enable = true;
  services.udiskie.enable = true;
  services.network-manager-applet.enable = true;
  services.screen-locker = {
    enable = true;
    lockCmd = "i3lock-color --clock -i $HOME/.background-lock --tiling";
    inactiveInterval = 15;
  };
  programs.firefox.enable = true;
  programs.termite = {
    enable = true;
    font = "Ubuntu Mono 16";
    sizeHints = true;
  };
  programs.rofi = {
    enable = true;
  };
  home.packages = with pkgs; [
    xdg-user-dirs
    xdg_utils
    youtube-dl
    spotify
    i3lock-color
  ];
  nixpkgs.config = {
    allowUnfree = true;
  };
}
