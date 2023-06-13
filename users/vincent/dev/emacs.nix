{ config, lib, pkgs, ... }:

with lib;
let
  capture = pkgs.writeScriptBin "capture" ''
    #!${pkgs.stdenv.shell}
    emacsclient -n -F '((name . "capture") (width . 150) (height . 90))' -e '(org-capture)'
  '';
  et = pkgs.writeScriptBin "et" ''
    #!${pkgs.stdenv.shell}
    emacsclient --tty $@
  '';
  ec = pkgs.writeScriptBin "ec" ''
    #!${pkgs.stdenv.shell}
    emacsclient --create-frame $@
  '';
  myExtraPackages = epkgs: with epkgs; [
    # FIXME(vdemeester) once it is fixed, re-add
    # pkgs.dired-plus
    # org-transclusion
    #python-mode
    #whole-line-or-region
    ace-window
    adoc-mode
    aggressive-indent
    alert
    async
    avy
    bbdb
    beginend
    bongo
    cape
    color-identifiers-mode
    consult
    consult-dir
    consult-lsp
    corfu
    dap-mode
    dash
    delight
    dired-collapse
    dired-narrow
    dired-rsync
    dired-subtree
    diredfl
    dockerfile-mode
    easy-kill
    edit-indirect
    eldoc-box
    elfeed
    elfeed-org
    embark
    embark-consult
    envrc
    esh-autosuggest
    eshell-prompt-extras
    esup
    expand-region
    flimenu
    flymake-languagetool
    focus
    git-annex
    git-commit
    git-gutter
    git-gutter-fringe
    git-modes
    github-review
    go-mode
    gotest
    goto-last-change
    hardhat
    helpful
    highlight
    highlight-indentation
    hl-todo
    htmlize
    ibuffer-vc
    icomplete-vertical
    json-mode
    kind-icon
    lin
    lsp-focus
    lsp-mode
    lsp-ui
    magit
    magit-annex
    magit-popup
    marginalia
    markdown-mode
    minions
    modus-themes
    moody
    multi-vterm
    mwim
    nix-buffer
    nix-mode
    nixpkgs-fmt
    no-littering
    ob-async
    ob-go
    ob-http
    orderless
    org
    orgalist
    org-appear
    org-capture-pop-frame
    org-contrib
    org-journal
    org-ql
    org-super-agenda
    org-tree-slide
    org-web-tools
    orgit
    ox-pandoc
    pandoc-mode
    pdf-tools
    popper
    pkgs.bookmark-plus
    rainbow-delimiters
    rainbow-mode
    rg
    ripgrep
    scratch
    shr-tag-pre-highlight
    smartparens
    symbol-overlay
    trashed
    treesit-auto
    try
    undo-tree
    use-package
    vertico
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
  home.file.".config/emacs" = {
    source = config.lib.file.mkOutOfStoreSymlink "/home/vincent/src/home/tools/emacs";
    # recursive = true;
  };
  home.file.".local/share/applications/org-protocol.desktop".source = ./emacs/org-protocol.desktop;
  home.file.".local/share/applications/capture.desktop".source = ./emacs/capture.desktop;
  home.packages = with pkgs; [
    ditaa
    graphviz
    pandoc
    sqlite
    zip
    ugrep
    # See if I can hide this under an option
    capture
    ec
    et
    languagetool
    asciidoctor
  ];
  programs.emacs = {
    enable = true;
    # FIXME: choose depending on the enabled modules
    package = (pkgs.emacs-pgtk.override { withGTK3 = true; withGTK2 = false; });
    # package = (pkgs.emacsNativeComp.override { withGTK3 = true; withGTK2 = false; withX = true; withXwidgets = true; });
    extraPackages = myExtraPackages;
  };
  services.emacs = {
    enable = true;
    client.enable = true;
    #socketActivation.enable = true;
  };
  home.sessionVariables = {
    EDITOR = "emacs";
    ALTERNATE_EDITOR = "emacs -nw";
  };
}
