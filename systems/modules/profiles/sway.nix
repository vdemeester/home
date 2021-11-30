{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.desktop.sway;
in
{
  options = {
    profiles.desktop.sway = {
      enable = mkEnableOption "Enable sway desktop profile";
    };
  };

  config = mkIf cfg.enable {
    #profiles = {
    #  desktop.enable = true;
    #};
    profiles.avahi.enable = true;
    profiles.printing.enable = true;
    profiles.pulseaudio.enable = true;
    profiles.scanning.enable = true;
    profiles.syncthing.enable = true;

    hardware.bluetooth.enable = true;

    networking.networkmanager = {
      enable = true;
      unmanaged = [
        "interface-name:br-*"
        "interface-name:ve-*"
        "interface-name:veth*"
        "interface-name:wg0"
        "interface-name:docker0"
        "interface-name:virbr*"
      ]; # FIXME: add unmanaged depending on profiles (wg0, docker0, …)
      packages = with pkgs; [ networkmanager-openvpn ];
    };

    # configuring sway itself (assmung a display manager starts it)
    systemd.user.targets.sway-session = {
      description = "Sway compositor session";
      documentation = [ "man:systemd.special(7)" ];
      bindsTo = [ "graphical-session.target" ];
      wants = [ "graphical-session-pre.target" ];
      after = [ "graphical-session-pre.target" ];
    };
    programs.sway = {
      enable = true;
      wrapperFeatures.gtk = true;
      extraPackages = with pkgs; [
        alacritty
        swaylock
        swayidle
        dmenu
        wofi
        xwayland
        mako
        kanshi
        grim
        slurp
        wl-clipboard
        wf-recorder
      ];
      extraSessionCommands = ''
        export SDL_VIDEODRIVER=wayland
        export QT_QPA_PLATFORM=wayland
        export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
        export _JAVA_AWT_WM_NONREPARENTING=1
        export MOZ_ENABLE_WAYLAND=1
      '';
    };
    # configuring kanshi
    #systemd.user.services.kanshi = {
    #  description = "Kanshi output autoconfig ";
    #  wantedBy = [ "graphical-session.target" ];
    #  partOf = [ "graphical-session.target" ];
    #  environment = { XDG_CONFIG_HOME = "/home/vincent/.config"; };
    #  serviceConfig = {
    #    # kanshi doesn't have an option to specifiy config file yet, so it looks
    #    # at .config/kanshi/config
    #    ExecStart = ''
    #      ${pkgs.kanshi}/bin/kanshi
    #    '';
    #    RestartSec = 5;
    #    Restart = "always";
    #  };
    #};

    services.xserver.enable = true;
    services.xserver.displayManager.defaultSession = "sway";
    services.xserver.layout = "fr";
    services.xserver.xkbVariant = "bepo";
    services.xserver.displayManager.sddm.enable = true;
    services.xserver.libinput.enable = true;
  };
}
