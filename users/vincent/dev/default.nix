{ config, pkgs, ... }:

{
  imports = [
    ./ai.nix
    ./go.nix
    ./js.nix
    ./lisp.nix
    ./mr.nix
    ./nix.nix
    ./python.nix
  ];

  home.extraOutputsToInstall = [
    "doc"
    "info"
    "devdoc"
  ];

  home.sessionVariables = {
    CARGO_HOME = "${config.xdg.dataHome}/cargo";
  };
  home.packages = with pkgs; [
    binutils
    cmake
    codespell
    # devenv
    difftastic
    fswatch
    gnumake
    gron
    gum
    ijq
    jq
    markdownlint-cli
    minica
    moreutils
    pre-commit
    shellcheck
    shfmt
    tldr
    tmate
    vale
    yamllint
    yamlfmt
    yq-go
    radicle-node
    vscode-fhs
  ];

  home.file.".ignore".text = ''
    *.swp
    *~
    **/VENDOR-LICENSE
  '';

  home.file.gdbinit = {
    target = ".gdbinit";
    text = ''
      set auto-load safe-path /
    '';
  };

}
