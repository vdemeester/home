{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/containers.nix
    ../../home/common/dev/tektoncd.nix
  ];
  nixpkgs.config.allowUnfree = true;

  home.file.".gmailctl/config.jsonnet".source = ./config.jsonnet;
  home.file.".gmailctl/gmailctl.libsonnet".source = ./gmailctl.libsonnet;
  home.packages = with pkgs; [
    chromium
    spotify
    easyeffects
    thunderbird
    nautilus

    slack

    gmailctl
    gcalcli

    ntfy-sh

    monolith # TODO: move into =desktop= ?

    did

    # Keyboard
    qmk
    qmk_hid

    beancount
    beancount-language-server
    beanquery
    beanhub-cli
    fava

    batzconverter

    simple-scan
    keybase

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
