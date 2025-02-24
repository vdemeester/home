{ config, lib, pkgs, ... }:
let
  inherit (lib) mkIf mkEnableOption mkDefault mkForce versionOlder;
  cfg = config.modules.desktop.wayland;
  stable = versionOlder config.system.nixos.release "24.05";
  swayRun = pkgs.writeShellScript "sway-run" ''
    export XDG_SESSION_TYPE=wayland
    export XDG_SESSION_DESKTOP=sway
    export XDG_CURRENT_DESKTOP=sway

    systemd-run --user --scope --collect --quiet --unit=sway systemd-cat --identifier=sway ${pkgs.sway}/bin/sway $@
  '';
in
{
  options = {
    modules.desktop.wayland = {
      enable = mkEnableOption "Enable wayland desktop";
    };
  };
  config = mkIf cfg.enable {
    # Enable desktop module if not already.
    modules.desktop.enable = true;
    # Force disable xorg desktop module
    modules.desktop.xorg.enable = mkForce false;
    # Hardware Support for Wayland Sway, …
    hardware = {
      # graphics
      opengl = {
        enable = true;
      };
    };
    services = {
      greetd = {
        enable = true;
        settings = {
          default_session = {
            # command = "${pkgs.greetd.greetd}/bin/agreety --cmd sway";
            command = "${lib.makeBinPath [ pkgs.greetd.tuigreet ]}/tuigreet --time --cmd ${swayRun}";
            users = "greeter";
          };
          initial_session = {
            command = "${swayRun}";
            user = "vincent";
          };
        };
        # restart = false;
      };
    } // (if stable then { } else {
      libinput = {
        touchpad = {
          disableWhileTyping = true;
          additionalOptions = ''
            							Option "Ignore" "on"
            						'';
        };
      };
    });
    environment.systemPackages = with pkgs; [
      qogir-icon-theme
    ];
  };
}
