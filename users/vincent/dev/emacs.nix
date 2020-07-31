{ config, lib, pkgs, ... }:

with lib;
let
  capture = pkgs.writeScriptBin "capture" ''
    #!${pkgs.stdenv.shell}
    emacsclient -s /run/user/1000/emacs/org -n -F '((name . "capture") (width . 150) (height . 90))' -e '(org-capture)'
  '';
  e = pkgs.writeScriptBin "e" ''
    #!${pkgs.stdenv.shell}
    emacs --dump-file=~/.config/emacs/emacs.pdmp $@
  '';
  et = pkgs.writeScriptBin "et" ''
    #!${pkgs.stdenv.shell}
    emacsclient -s /run/user/1000/emacs/org --tty $@
  '';
  ec = pkgs.writeScriptBin "ec" ''
    #!${pkgs.stdenv.shell}
    emacsclient -s /run/user/1000/emacs/org --create-frame $@
  '';
  myExtraPackages = epkgs: with epkgs; [
    ace-window
    aggressive-indent
    async
    avy
    bbdb
    beginend
    color-identifiers-mode
    company
    company-emoji
    company-go
    dash
    delight
    dired-collapse
    dired-git-info
    dired-narrow
    dired-quick-sort
    dired-rsync
    dockerfile-mode
    dumb-jump
    easy-kill
    edit-indirect
    envrc
    esh-autosuggest
    eshell-prompt-extras
    esup
    expand-region
    flimenu
    flycheck
    flycheck-golangci-lint
    git-annex
    git-commit
    gitattributes-mode
    gitconfig-mode
    github-review
    gitignore-mode
    go-mode
    gotest
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
    magit
    magit-annex
    magit-popup
    magit-todos
    markdown-mode
    minions
    modus-operandi-theme
    moody
    mwim
    nix-buffer
    nix-mode
    nixpkgs-fmt
    no-littering
    ob-async
    ob-go
    ob-http
    olivetti
    orderless
    org-capture-pop-frame
    org-gcal
    org-journal
    org-plus-contrib
    org-ql
    org-ref
    org-roam
    org-super-agenda
    org-superstar
    org-tree-slide
    org-web-tools
    orgit
    ox-pandoc
    pandoc-mode
    pdf-tools
    pkgs.bookmark-plus
    pkgs.dired-plus
    projectile
    python-mode
    rainbow-delimiters
    rainbow-mode
    rg
    ripgrep
    smartparens
    symbol-overlay
    try
    undo-tree
    use-package
    visual-fill-column
    visual-regexp
    vterm
    web-mode
    wgrep
    with-editor
    xterm-color
    yaml-mode
  ];
in
{
  home.file.".local/share/applications/org-protocol.desktop".source = ./emacs/org-protocol.desktop;
  home.file.".local/share/applications/ec.desktop".source = ./emacs/ec.desktop;
  home.file.".local/share/applications/capture.desktop".source = ./emacs/capture.desktop;
  home.packages = with pkgs; [
    ditaa
    graphviz
    pandoc
    sqlite
    zip
    # See if I can hide this under an option
    capture
    e
    ec
    et
  ];
  programs.emacs = {
    enable = true;
    package = pkgs.my.emacs;
    extraPackages = myExtraPackages;
  };
  services.emacs-server = {
    enable = true;
    package = pkgs.my.emacs;
    name = "org";
    shell = pkgs.zsh + "/bin/zsh -i -c";
    # FIXME do this in the derivation :)
    # extraOptions = "--dump-file=${config.home.homeDirectory}/.config/emacs/emacs.pdmp";
  };
  home.sessionVariables = {
    EDITOR = "et";
    ALTERNATE_EDITOR = "et";
  };
}
