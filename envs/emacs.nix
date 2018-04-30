{ pkgs, config, lib, melpaBuild, ... }:
    
let  
compileEmacsFiles  = pkgs.callPackage ./emacs/builder.nix;

/*
counsel-org-clock= compileEmacsFiles {
  name = "counsel-org-clock";
  src = pkgs.fetchFromGitHub {
    owner = "akirak";
    repo = "counsel-org-clock";
    rev = "18d68c7ce7b461bb7055ff873e39d5849a971417";
    sha256 = "0c9yiwrc6apxrhc8dyfdgs6q2m2l8rd92rwmywci7ivhcpp4cadi";
  };
};
*/

bookmark-plus = compileEmacsFiles {
  name = "bookmark-plus";
  src = pkgs.fetchFromGitHub {
    owner = "emacsmirror";
    repo = "bookmark-plus";
    rev = "954d48092247e9fd461d310fd9fc3c0d545362d5";
    sha256 = "0c9yiwrc6apxrrc8dyfdgs6q2m2l8rd92rwmywci7ivhcpp4cadi";
  };
};

/*
minions = pkgs.melpaBuild {
  pname = "minions";
  version = "20180321.749";
  src = pkgs.fetchFromGitHub {
    owner = "tarsius";
    repo = "minions";
    rev = "acac7fb0b04ffdf243775231690956d163474177";
    sha256 = "1065asbg1h2chd34cbja2pir93r5y8gp3wv3jv6sf5csa6yqk6c7";
  };
  recipeFile = pkgs.fetchurl {
    url = "https://raw.githubusercontent.com/melpa/melpa/e5cfaa4b5fda97054d45691fad9d79b559f2df14/recipes/minions";
    sha256 = "1065asbg1h2chd34cbja2pir93r5y8gp3wv3jv6sf5csa6yqk6ch";
    name = "minions";
  };
  packageRequires = [ pkgs.emacs ];
  meta = {
    homepage = "https://melpa.org/#/elfeed";
    license = lib.licenses.free;
  };
};
*/

in {
  
  programs.emacs = {
    enable = true;
    extraPackages = epkgs: with epkgs; [
      ace-window
      aggressive-indent
      alert
      async
      auto-yasnippet
      avy
      bm
      bookmark-plus
      command-log-mode
      company
      company-emoji
      company-ghc
      company-go
      company-lsp
      company-nixos-options
      company-restclient
      company-shell
      counsel
      counsel-gtags
      # counsel-org-clock # FIXME
      counsel-projectile
      # counsel-spotify
      counsel-tramp
      crux
      dash
      delight
      diff-hl
      diffview
      dired-collapse
      direnv
      docker
      docker-compose-mode
      docker-tramp
      dockerfile-mode
      doom-themes
      dumb-jump
      elpy
      engine-mode
      eshell-bookmark
      eshell-prompt-extras
      esh-autosuggest
      exec-path-from-shell
      expand-region
      eyebrowse
      ez-query-replace
      fancy-narrow
      fish-mode
      fish-completion
      flycheck
      flycheck-clojure
      flycheck-haskell
      # flycheck-inline
      flycheck-gometalinter
      flycheck-popup-tip
      focus
      fullframe
      ggtags
      ghub
      ghub-plus
      git-commit
      git-timemachine
      gitattributes-mode
      gitconfig-mode
      gitignore-mode
      go-mode
      go-add-tags
      # go-dlv
      go-eldoc
      go-errcheck
      go-fill-struct
      go-guru
      # go-impl
      gorepl-mode
      go-tag
      groovy-mode
      hardhat
      haskell-mode
      helpful
      highlight
      highlight-escape-sequences
      highlight-leading-spaces
      highlight-numbers
      highlight-symbol
      hydra
      ialign
      ibuffer-vc
      ibuffer-projectile
      ibuffer-sidebar
      iedit
      imenu-list
      ivy
      ivy-historian
      ivy-hydra
      ivy-pass
      ivy-rich
      jedi
      jq-mode
      js2-mode
      js2-refactor
      json-mode
      json-reformat
      json-snatcher
      lsp-haskell
      lsp-javascript-typescript
      lsp-go
      lsp-mode
      lsp-python
      lsp-rust
      magit
      magit-filenotify
      magit-gitflow
      magit-lfs
      magit-popup
      magithub
      makefile-executor
      markdown-mode
      mc-extras
      # minions # FIXME
      multiple-cursors
      nix-buffer
      nix-mode
      nix-sandbox
      nixos-options
      no-littering
      olivetti
      ob-go
      ob-restclient
      ob-rust
      ob-typescript
      org-plus-contrib
      org-ref
      org-super-agenda
      org-web-tools
      ox-epub
      ox-hugo
      ox-ioslide
      ox-pandoc
      ox-tufte
      ox-twbs
      pandoc-mode
      pass
      password-store
      password-store-otp
      persistent-scratch
      popup
      projectile
      projectile-ripgrep
      python-mode
      rainbow-delimiters
      rainbow-mode
      request
      restclient
      ripgrep
      selected
      shift-number
      shackle
      shx
      smart-jump
      smart-newline
      smart-region
      smartparens
      solaire-mode
      sort-words
      sos
      string-edit
      string-inflection
      super-save
      sx
      tide
      toml-mode
      typescript-mode
      undo-tree
      use-package
      vdiff
      visual-fill-column
      visual-regexp
      visual-regexp-steroids
      vlf
      web-mode
      wgrep
      which-key
      whitespace-cleanup-mode
      # window-purpose
      with-editor
      yasnippet
      yaml-mode
    ];
  };
}
