{ config, lib, pkgs, ... }:
with lib;
let
  cfg = config.services.emacs-server;
in
{
  options.services.emacs-server = {
    enable = mkEnableOption "the Emacs daemon";
    name = mkOption {
      type = types.str;
      description = "Name of the emacs server";
      default = "default";
    };
    package = mkOption {
      type = types.package;
      description = "The Emacs package to use for running the daemon.";
    };
    shell = mkOption {
      type = types.str;
      description = "The shell used for starting Emacs.";
    };
    extraOptions = mkOption {
      type = types.separatedString " ";
      default = "";
      description =
        ''
          The extra command-line options to pass to
          <command>emacs</command> daemon.
        '';
    };
  };

  config = mkIf cfg.enable {
    systemd.user.services.emacs-server = {
      Unit = {
        Description = "Emacs: the extensible, self-documenting text editor";
        Documentation =
          "info:emacs man:emacs(1) https://gnu.org/software/emacs/";

        # Avoid killing the Emacs session, which may be full of
        # unsaved buffers.
        X-RestartIfChanged = false;
      };

      Service = {
        ExecStart =
          "${cfg.shell} 'exec emacs --fg-daemon=${cfg.name} ${cfg.extraOptions}'";
        ExecStop = "${cfg.shell} 'emacsclient --eval '(kill-emacs)''";
        Restart = "on-failure";
        Environment = "DISPLAY=:0";
      };

      Install = { WantedBy = [ "default.target" ]; };
    };
  };
}
