{ pkgs, ... }:
{
  imports = [
    ../../home/common/dev/containers.nix
    ../../home/common/dev/tektoncd.nix
    ../../home/common/services/color-scheme-timer.nix
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

    finamp

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

    # lisp
    roswell
    sbcl

    go-org-readwise
    gh-restart-failed
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
