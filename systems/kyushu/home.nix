{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/containers.nix
  ];

  home.file.".gmailctl/config.jsonnet".source = ./config.jsonnet;
  home.file.".gmailctl/gmailctl.libsonnet".source = ./gmailctl.libsonnet;
  home.packages = with pkgs; [
    gmailctl

    # lisp
    roswell
    sbcl
  ];

  # systemd.user.services.battery-monitor = {
  #   Unit = {
  #     Description = "battery monitory service";
  #     After = "graphical-session.target";
  #     PartOf = "graphical-session.target";
  #
  #     # Avoid killing the Emacs session, which may be full of
  #     # unsaved buffers.
  #     X-RestartIfChanged = false;
  #   };
  #   Service = {
  #     ExecStart = ''
  #       ${pkgs.battery-monitor}/bin/battery-monitor
  #     '';
  #     Restart = "on-failure";
  #   };
  #   Install = {
  #     WantedBy = [ "graphical-session.target" ];
  #   };
  # };
}
