{
  config,
  lib,
  pkgs,
  ...
}:
let
  capture = pkgs.writeScriptBin "capture" ''
    #!${pkgs.stdenv.shell}
    emacsclient -n -c -F '((name . "capture") (width . 150) (height . 90) (vde/window-popup-frame . t))' -e '(org-capture)'
  '';
  et = pkgs.writeScriptBin "et" ''
    #!${pkgs.stdenv.shell}
    emacsclient --tty $@
  '';
  ec = pkgs.writeScriptBin "ec" ''
    #!${pkgs.stdenv.shell}
    emacsclient --create-frame $@
  '';
  myExtraPackages =
    epkgs: with epkgs; [
      ace-window
      adoc-mode
      age
      aggressive-indent
      aidermacs
      alert
      async
      avy
      beginend
      cape
      casual
      casual-avy
      conner
      consult
      consult-dir
      consult-denote
      consult-project-extra
      consult-vc-modified-files
      copilot
      copilot-chat
      corfu
      corfu-candidate-overlay
      dape
      dash
      denote
      denote-org
      denote-journal
      denote-sequence # maybe ?
      denote-menu
      devdocs
      diff-hl
      dired-collapse
      dired-narrow
      dired-rsync
      diredfl
      dockerfile-mode
      doom-modeline
      easy-kill
      eat
      edit-indirect
      editorconfig
      eldoc-box
      pr-review
      embark
      embark-consult
      emms
      envrc
      eshell-atuin
      eshell-prompt-extras
      esup
      flimenu
      flymake-yamllint
      git-modes
      go-mode
      gotest
      gotest-ts
      gptel
      hardhat
      helpful
      highlight
      highlight-indentation
      htmlize
      ibuffer-vc
      indent-bars
      jinx
      jira
      json-mode
      kubed
      ligature
      macrostep
      magit
      magit-popup
      marginalia
      markdown-mode
      minions
      modus-themes
      multi-vterm
      mu4e
      mwim
      nix-mode
      nix-ts-mode
      nixpkgs-fmt
      no-littering
      noether
      # notmuch
      ob-async
      ob-go
      ob-http
      orderless # TODO configure this
      org
      org-contrib
      org-download
      org-modern
      org-nix-shell
      org-ql
      org-review
      org-rich-yank
      org-tree-slide
      org-web-tools
      orgalist
      orgit
      outline-indent
      ox-pandoc
      pandoc-mode
      # password-store
      # pkgs.bookmatrk-plus # Do I use it ?
      popper
      project-rootfile
      rg
      run-command # Try this out instead of conner, might be even better
      scopeline
      scratch
      shr-tag-pre-highlight
      sideline
      sideline-eglot
      sideline-flymake
      smartparens
      substitute
      surround
      symbol-overlay
      tempel
      tempel-collection
      topsy
      trashed
      treesit-fold
      treesit-grammars.with-all-grammars # see how much it weight
      typescript-mode
      vc-jj
      verb
      vertico
      visual-fill-column
      visual-regexp
      vterm
      vundo
      web-mode
      wgrep
      with-editor
      xterm-color
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
    #package = (pkgs.emacs29.override { withTreeSitter = true; withNativeCompilation = true; withPgtk = true; withWebP = true; withGTK3 = true; withSQLite3 = true; });
    package = pkgs.emacs-unstable.override {
      withTreeSitter = true;
      withNativeCompilation = true;
      withPgtk = true;
      withWebP = true;
      withGTK3 = true;
      withSQLite3 = true;
    };
    extraPackages = myExtraPackages;
  };
  # services.emacs = {
  #   enable = true;
  #   client.enable = true;
  #   #socketActivation.enable = true;
  # };
  home.sessionVariables = {
    EDITOR = "emacs";
    ALTERNATE_EDITOR = "emacs -nw";
  };
}
