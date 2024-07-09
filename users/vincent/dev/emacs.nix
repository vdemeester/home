{ config, lib, pkgs, ... }:

with lib;
let
  capture = pkgs.writeScriptBin "capture" ''
    #!${pkgs.stdenv.shell}
    emacsclient -n -c -F '((name . "capture") (width . 150) (height . 90))' -e '(org-capture)'
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
    # python-mode
    # whole-line-or-region
    # bongo
    # git-annex
    # github-review
    # edit-indirect
    # kind-icon
    # dired-subtree
    # lsp-focus
    # lsp-mode
    # lsp-ui
    # consult-lsp
    # dap-mode
    # icomplete-vertical
    ace-window
    adoc-mode
    aggressive-indent
    alert
    # all-the-icons
    async
    avy
    bbdb
    beginend
    cape
    casual-avy
    casual-dired
    # color-identifiers-mode
    conner
    consult
    consult-dir
    consult-notes
    corfu
    dash
    dape
    # delight
    denote
    dired-collapse
    dired-narrow
    dired-rsync
    # dired-sidebar
    diredfl
    dockerfile-mode
    doom-modeline
    eat
    easy-kill
    editorconfig
    edit-indirect
    eldoc-box
    # emacs-everywhere
    embark
    embark-consult
    # emms
    envrc
    eshell-prompt-extras
    esup
    # expand-region
    flimenu
    # flymake-languagetool
    flymake-yamllint
    # flymake-codespell
    # fontaine
    focus
    git-commit
    git-gutter
    git-gutter-fringe
    git-modes
    go-mode
    gotest
    # goto-last-change
    hardhat
    helpful
    highlight
    highlight-indentation
    # hl-todo
    htmlize
    ibuffer-vc
    jinx
    json-mode
    ligature
    # lin
    macro-expand
    magit
    magit-popup
    marginalia
    markdown-mode
    mct
    modus-themes
    multi-vterm
    # multiple-cursors
    mwim
    # nerd-icons
    # nix-buffer
    nix-mode
    nix-ts-mode
    nixpkgs-fmt
    no-littering
    ob-async
    ob-go
    ob-http
    orderless # TODO configure this
    org
    orgalist
    # org-appear
    # org-capture-pop-frame
    org-contrib
    # org-journal
    org-modern
    org-nix-shell
    org-ql
    org-rich-yank
    # org-super-agenda
    org-tree-slide
    org-web-tools
    orgit
    ox-pandoc
    pandoc-mode
    # pdf-tools
    popper
    pkgs.bookmark-plus # Do I use it ?
    # rainbow-delimiters
    # rainbow-mode
    rg
    ripgrep
    run-command # Try this out instead of conner, might be even better
    scratch
    shr-tag-pre-highlight
    smartparens
    substitute
    surround
    symbol-overlay
    tempel
    tempel-collection
    trashed
    treesit-auto
    # treesit-grammars.with-all-grammars
    try
    typescript-mode
    # undo-tree
    # use-package # it's now part of built-in packages
    visual-fill-column
    visual-regexp
    vterm
    vundo
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
    # github-copilot-cli # for copilot.el
    nodejs
    ec
    et
    languagetool
    asciidoctor
    enchant
  ];
  programs.emacs = {
    enable = true;
    # FIXME: choose depending on the enabled modules
    package = (pkgs.emacs29.override { withTreeSitter = true; withNativeCompilation = true; withPgtk = true; withWebP = true; withGTK3 = true; withGTK2 = false; withSQLite3 = true; });
    # package = (pkgs.emacs-pgtk.override { withGTK3 = true; withGTK2 = false; });
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
