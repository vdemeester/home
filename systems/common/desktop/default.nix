{ desktop
, pkgs
, self
, ...
}:
{
  imports = [
    (./. + "/${desktop}.nix")

    ./binfmt.nix
    ../programs/nix-ld.nix
    ../services/pipewire.nix
    # ../hardware/yubikey.nix
    # ../virt
  ];
  # Enable Plymouth and surpress some logs by default.
  # boot = {
  #   # initrd.systemd.enable = true;
  #   plymouth.enable = true;
  #   kernelParams = [
  #     # The 'splash' arg is included by the plymouth option
  #     "quiet"
  #     "loglevel=3"
  #     "rd.udev.log_priority=3"
  #     "vt.global_cursor_default=0"
  #   ];
  # };

  hardware.graphics.enable = true;

  # Enable location services
  location.provider = "geoclue2";

  environment.systemPackages = with pkgs; [
    bat # cat
    cyme # lsusb
    dig
    eza # ls
    fd # find
    duf # df
    ripgrep
  ];

  fonts = {
    fontDir.enable = true;
    enableGhostscriptFonts = true;
    packages = with pkgs; [
      cascadia-code
      corefonts
      dejavu_fonts
      # emojione
      feh
      fira
      fira-code
      fira-code-symbols
      fira-mono
      font-awesome
      go-font
      hack-font
      inconsolata
      jetbrains-mono
      liberation_ttf
      nerd-fonts.jetbrains-mono
      nerd-fonts.inconsolata
      nerd-fonts.fira-code
      nerd-fonts.fira-mono
      nerd-fonts.caskaydia-cove
      nerd-fonts.caskaydia-mono
      nerd-fonts.overpass
      nerd-fonts.ubuntu
      nerd-fonts.ubuntu-mono
      nerd-fonts.ubuntu-sans
      noto-fonts
      noto-fonts-cjk-sans
      noto-fonts-emoji
      noto-fonts-extra
      overpass
      symbola
      twemoji-color-font
      ubuntu_font_family
      unifont
      recursive
    ];

    # Use fonts specified by user rather than default ones
    enableDefaultPackages = false;

    # TODO configure thoses
    # fontconfig = {
    #   enable = true;
    #   defaultFonts = {
    #     serif = [
    #       "${theme.fonts.default.name}"
    #       "${theme.fonts.emoji.name}"
    #     ];
    #     sansSerif = [
    #       "${theme.fonts.default.name}"
    #       "${theme.fonts.emoji.name}"
    #     ];
    #     monospace = [ "${theme.fonts.monospace.name}" ];
    #     emoji = [ "${theme.fonts.emoji.name}" ];
    #   };
    # };
  };

  services = {
    envfs.enable = true;
    power-profiles-daemon.enable = true;
    udisks2.enable = true;

    # Make `/run/user/X` larger
    logind.extraConfig = ''
      			RuntimeDirectorySize=20%
      		'';
  };
}
