{ config, pkgs, ... }:
{
  imports = [
    ./ai.nix
    ./go.nix
    ./nix.nix
    ./python.nix
  ];

  home.sessionVariables = {
    CARGO_HOME = "${config.xdg.dataHome}/cargo";
  };

  home.packages = with pkgs; [
    jq
    ijq # interactive jq
    # gron # make json greppable

    binutils
    gnumake
    moreutils

    # Shell tooling
    shellcheck
    shfmt

    # try
    jujutsu
    jj-fzf
    devenv

    # old
    cmake
    codespell
    # devenv
    difftastic
    fswatch
    gum
    markdownlint-cli
    minica
    pre-commit
    tldr
    tmate
    vale
    yamllint
    yamlfmt
    yq-go
    # radicle-node
    vscode-fhs

    bash-language-server
  ];

}
