{ pkgs, ... }:
let
  # Wrapper for jira-cli that injects API token from passage
  jira-wrapped = pkgs.writeShellScriptBin "jira" ''
    export JIRA_API_TOKEN=$(${pkgs.passage}/bin/passage show redhat/issues/token/kyushu)
    exec ${pkgs.jira-cli-go}/bin/jira "$@"
  '';
in
{
  imports = [
    ../../home/common/dev/containers.nix
    ../../home/common/dev/tektoncd.nix
    ../../home/common/services/color-scheme-timer.nix
    (import ../../home/common/services/beets.nix { }) # Use default baseDir
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

    calibre

    ntfy-sh

    monolith # TODO: move into =desktop= ?

    did

    finamp # jellyfin
    feishin # navidrome

    # Keyboard
    qmk
    qmk_hid

    beancount
    beancount-language-server
    beanquery
    beanhub-cli
    fava

    startpaac

    batzconverter

    simple-scan
    keybase

    transmission_4-gtk

    forgejo-cli
    jira-wrapped

    # lisp
    roswell
    sbcl

    go-org-readwise
    gh-pr
    arr
    claude-hooks
    toggle-color-scheme
  ];

  # Automatic color scheme switching
  services.color-scheme-timer = {
    enable = true;
    latitude = "48.87"; # Paris coordinates
    longitude = "2.33";
    lightTime = "07:00"; # Switch to light mode at 7am
    darkTime = "19:00"; # Switch to dark mode at 7pm
  };

}
