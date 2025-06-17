{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/containers.nix
  ];

  systemd.user.services.battery-monitor = {
    Unit = {
      Description = "battery monitory service";
      After = "graphical-session.target";
      PartOf = "graphical-session.target";

      # Avoid killing the Emacs session, which may be full of
      # unsaved buffers.
      X-RestartIfChanged = false;
    };
    Service = {
      ExecStart = ''
        ${pkgs.battery-monitor}
      '';
      Restart = "on-failure";
    };
    Install = {
      WantedBy = [ "graphical-session.target" ];
    };
  };
}
