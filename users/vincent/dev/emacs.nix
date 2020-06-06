{ config, lib, pkgs, ... }:

with lib;
let
  capture = pkgs.writeScriptBin "capture" ''
    #!${pkgs.stdenv.shell}
    emacsclient -s /run/user/1000/emacs/org -n -F '((name . "capture") (width . 150) (height . 90))' -e '(org-capture)'
  '';
in
{
  home.file.".local/share/applications/org-protocol.desktop".source = ./emacs/org-protocol.desktop;
  home.file.".local/share/applications/ec.desktop".source = ./emacs/ec.desktop;
  home.file.".local/share/applications/capture.desktop".source = ./emacs/capture.desktop;
  home.packages = with pkgs; [
    ditaa
    graphviz
    pandoc
    zip
    # See if I can hide this under an option
    capture
  ];
  home.sessionVariables = {
    EDITOR = "et";
    ALTERNATE_EDITOR = "et";
  };
  programs.emacs = {
    enable = true;
    package = pkgs.my.emacs;
    extraPackages = epkgs: with epkgs; [
      ace-window
      aggressive-indent
      async
      avy
      bbdb
      beginend
      pkgs.bookmark-plus
      company
      company-emoji
      company-go
      dash
      delight
      dired-collapse
      dired-git-info
      dired-quick-sort
      dired-narrow
      dired-rsync
      pkgs.dired-plus
      dumb-jump
      direnv
      dockerfile-mode
      easy-kill
      esup
      expand-region
      flycheck
      flycheck-golangci-lint
      git-annex
      git-commit
      gitattributes-mode
      gitconfig-mode
      gitignore-mode
      github-review
      goto-last-change
      hardhat
      helpful
      highlight
      highlight-indentation
      highlight-numbers
      ibuffer-vc
      icomplete-vertical
      iedit
      json-mode
      markdown-mode
      mpdel
      multiple-cursors
      nixpkgs-fmt
      no-littering
      ob-async
      ob-go
      ob-http
      orderless
      orgit
      org-plus-contrib
      org-capture-pop-frame
      org-gcal
      org-ql
      org-ref
      org-super-agenda
      org-web-tools
      ox-pandoc
      pandoc-mode
      projectile
      projectile-ripgrep
      pdf-tools
      python-mode
      rainbow-delimiters
      rainbow-mode
      region-bindings-mode
      ripgrep
      rg
      try
      visual-fill-column
      visual-regexp
      web-mode
      wgrep
      with-editor
      xterm-color
      yaml-mode
      darkroom
      eshell-prompt-extras
      esh-autosuggest
      flimenu
      forge
      go-mode
      magit
      magit-annex
      magit-popup
      magit-todos
      minions
      moody
      mwim
      nix-buffer
      nix-mode
      org-super-agenda
      org-tree-slide
      shr-tag-pre-highlight
      ssh-config-mode
      smartparens
      symbol-overlay
      undo-tree
      use-package
      # Highly experimental
      vterm
      gotest
    ];
  };
  services.emacs-server = {
    enable = true;
    package = pkgs.my.emacs;
    name = "org";
    shell = pkgs.zsh + "/bin/zsh -i -c";
    # FIXME do this in the derivation :)
    extraOptions = "--dump-file=${config.home.homeDirectory}/.config/emacs/emacs.pdmp";
  };
}
