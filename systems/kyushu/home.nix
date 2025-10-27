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

}
