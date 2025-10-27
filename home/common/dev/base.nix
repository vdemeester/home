{ config, pkgs, ... }:
{
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
    # jj-fzf
    devenv
    chmouzies-git

    # old
    cmake
    codespell
    difftastic
    fswatch
    gum
    markdownlint-cli
    minica
    pre-commit
    # tldr
    # tmate
    vale
    yamllint
    yamlfmt
    yq-go
    # radicle-node

    bash-language-server
  ];

}
