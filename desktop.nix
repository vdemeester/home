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
  xdg.configFile."user-dirs.dirs".source = ./xorg/user-dirs.dirs;
  home.file.".local/share/applications/org-protocol.desktop".source = ./xorg/org-protocol.desktop;
  services = {
    gpg-agent = {
      enable = true;
      enableSshSupport = true;
      defaultCacheTtlSsh = 7200;
      extraConfig = ''
      allow-emacs-pinentry
      '';
    };
    dunst.enable = true;
    udiskie.enable = true;
    network-manager-applet.enable = true;
    screen-locker = {
      enable = true;
      lockCmd = "i3lock-color --clock --color=606060";
      inactiveInterval = 15;
    };
    random-background = {
      enable = true;
      imageDirectory = "/home/vincent/desktop/pictures/wallpapers/Unsplashed";
    };
  };
  programs = {
    firefox.enable = true;
    termite = {
      enable = true;
      font = "Ubuntu Mono 16";
      sizeHints = true;
    };
    rofi = {
      enable = true;
    };
  };
  xdg.configFile."alacritty/alacritty.yml".source = ./xorg/alacritty.yml;
  home.packages = with pkgs; [
    alacritty # create a `programs.alacritty`
    aspell
    aspellDicts.en
    aspellDicts.fr
    #etBook
    gnome3.defaultIconTheme
    gnome3.gnome_themes_standard
    i3lock-color
    keybase
    libnotify
    maim
    # pass
    peco
    # scrot
    spotify
    slop
    xdg-user-dirs
    xdg_utils
    youtube-dl
  ];
}
