{ configs, pkgs, ...}:

{
  imports = [
    ./default.nix
    ./printing.nix
    ./scanning.nix
    ./avahi.nix
    ./syncthing.nix
    ./fish.nix
  ];

  boot.loader.efi.canTouchEfiVariables = true;
  boot.tmpOnTmpfs = true;

  environment.systemPackages = with pkgs; [
    cryptsetup
    emacs
    xlibs.xmodmap
    xorg.xbacklight
    xorg.xdpyinfo
    xorg.xhost
    xorg.xinit
    xss-lock
    xorg.xmessage
    unzip
    gnupg
    pinentry
    mpv
  ];
  hardware.opengl.extraPackages = [ pkgs.vaapiIntel ];
  networking.networkmanager.enable = true;
  networking.networkmanager.unmanaged = [
    "interface-name:ve-*" "interface-name:veth*"
  ];
  services = {
    xserver = {
      enable = true;
      enableTCP = false;
      windowManager.twm.enable = true;
      libinput.enable = true;
      synaptics.enable = false;
      layout = "fr(bepo),fr";
      xkbVariant = "oss";
      xkbOptions = "grp:menu_toggle,grp_led:caps,compose:caps";
      inputClassSections = [
        ''
          Identifier      "TypeMatrix"
          MatchIsKeyboard "on"
          MatchVendor     "TypeMatrix.com"
          MatchProduct    "USB Keyboard"
          Driver          "evdev"
          Option          "XbkModel"      "tm2030USB"
          Option          "XkbLayout"     "fr"
          Option          "XkbVariant"    "bepo"
        ''
        ''
          Identifier      "ErgoDox"
          MatchIsKeyboard "on"
          #MatchVendor     "ErgoDox_EZ"
          #MatchProduct    "ErgoDox_EZ"
          MatchUSBID      "feed:1307"
          Driver          "evdev"
          Option          "XkbLayout"     "fr"
          Option          "XkbVariant"    "bepo"
        ''
      ];
      displayManager = {
        slim = {
          enable = true;
          # Probably put this into users instead ?
          defaultUser = "vincent";
        };
      };
    };
  };
  
  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      dejavu_fonts
      emojione
      feh
      fira
      fira-code
      fira-code-symbols
      fira-mono
      font-droid
      hasklig
      inconsolata
      iosevka
      symbola
      source-code-pro
      ubuntu_font_family
      unifont
    ];
  };
  
  
  # Polkit.
  security.polkit.extraConfig = ''
    polkit.addRule(function(action, subject) {
    if ((action.id == "org.freedesktop.udisks2.filesystem-mount-system" ||
    action.id == "org.freedesktop.udisks2.encrypted-unlock-system"
    ) &&
    subject.local && subject.active && subject.isInGroup("users")) {
    return polkit.Result.YES;
    }
    var YES = polkit.Result.YES;
    var permission = {
    // required for udisks1:
    "org.freedesktop.udisks.filesystem-mount": YES,
    "org.freedesktop.udisks.luks-unlock": YES,
    "org.freedesktop.udisks.drive-eject": YES,
    "org.freedesktop.udisks.drive-detach": YES,
    // required for udisks2:
    "org.freedesktop.udisks2.filesystem-mount": YES,
    "org.freedesktop.udisks2.encrypted-unlock": YES,
    "org.freedesktop.udisks2.eject-media": YES,
    "org.freedesktop.udisks2.power-off-drive": YES,
    // required for udisks2 if using udiskie from another seat (e.g. systemd):
    "org.freedesktop.udisks2.filesystem-mount-other-seat": YES,
    "org.freedesktop.udisks2.filesystem-unmount-others": YES,
    "org.freedesktop.udisks2.encrypted-unlock-other-seat": YES,
    "org.freedesktop.udisks2.eject-media-other-seat": YES,
    "org.freedesktop.udisks2.power-off-drive-other-seat": YES
    };
    if (subject.isInGroup("wheel")) {
    return permission[action.id];
    }
    });
  '';
  # Auto refresh nix-channel each day
  systemd.user.services.channel-update = {
    description = "Update nix-channel daily";
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "/run/current-system/sw/bin/nix-channel --update";
      Environment = "PATH=/run/current-system/sw/bin";
    };
  };
  systemd.user.timers.channel-update = {
    description = "Update nix-channel daily";
    wantedBy = [ "timers.target" ];
    timerConfig = {
      OnCalendar = "daily";
      Persistent = "true";
    };
  };
  systemd.user.timers.channel-update.enable = true;
}
