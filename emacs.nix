{ pkgs, config, lib, ... }:

{
  systemd.user.services.emacs = {
    Unit = {
      Description = "Emacs: the extensible, self-documenting text editor";
    };
    Service = {
      Environment = ''
        PATH=/home/vincent/bin:/home/vincent/.local/npm/bin:/run/wrappers/bin:/etc/profiles/per-user/vincent/bin:${config.home.profileDirectory}/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin GOPATH=/home/vincent ASPELL_CONF=dict-dir=/home/vincent/.nix-profile/lib/aspell
      '';
      Type      = "forking";
      ExecStart = "${pkgs.bash}/bin/bash -c 'source /etc/profile; exec /home/vincent/.nix-profile/bin/emacs --daemon'";
      ExecStop  = "/home/vincent/.nix-profile/bin/emacsclient --eval (kill-emacs)";
      Restart   = "always";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  systemd.user.services.emacs-org = {
    Unit = {
      Description = "Emacs: the extensible, self-documenting text editor";
    };
    Service = {
      Environment = ''
        PATH=/home/vincent/bin:/home/vincent/.local/npm/bin:/run/wrappers/bin:/etc/profiles/per-user/vincent/bin:${config.home.profileDirectory}/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin GOPATH=/home/vincent ASPELL_CONF=dict-dir=/home/vincent/.nix-profile/lib/aspell
      '';
      Type      = "forking";
      ExecStart = "${pkgs.bash}/bin/bash -c 'source /etc/profile; exec /home/vincent/.nix-profile/bin/emacs --daemon=org'";
      ExecStop  = "/home/vincent/.nix-profile/bin/emacsclient --socket-name=org --eval (kill-emacs)";
      Restart   = "always";
    };
    Install = {
      WantedBy = [ "default.target" ];
    };
  };
  programs.emacs = {
    enable = true;
    # package = pkgs.myEmacs;
    extraPackages = epkgs: with epkgs; [
      ace-window
      aggressive-indent
      # alert
      async
      # auto-yasnippet
      avy
      bm
      command-log-mode
      company
      company-emoji
      company-ghc
      company-go
      company-lsp
      (with melpaPackages; [ company-nixos-options ])
      #company-restclient
      #company-shell
      counsel
      #counsel-gtags
      counsel-projectile
      # counsel-spotify
      #counsel-tramp
      #crux
      dash
      delight
      diff-hl
      diffview
      dired-collapse
      dired-sidebar
      direnv
      docker
      docker-compose-mode
      dockerfile-mode
      doom-themes
      #dumb-jump
      #elpy
      eshell-bookmark
      (with melpaPackages; [
      eshell-prompt-extras
      esh-autosuggest
      ])
      exec-path-from-shell
      expand-region
      eyebrowse
      (with melpaPackages; [ ez-query-replace ])
      fancy-narrow
      fish-mode
      (with melpaPackages; [ fish-completion ])
      flycheck
      #flycheck-clojure
      #flycheck-haskell
      #flycheck-inline
      #flycheck-gometalinter
      flycheck-popup-tip
      focus
      fullframe
      #ggtags
      #ghub
      #ghub-plus
      git-commit
      #git-timemachine
      gitattributes-mode
      gitconfig-mode
      gitignore-mode
      (with melpaPackages; [
        go-add-tags
        # go-dlv
        go-eldoc
        go-errcheck
        go-fill-struct
        go-guru
        # go-impl
        go-mode
        #gorepl-mode
        go-tag
        gotest
      ])
      groovy-mode
      hardhat
      #haskell-mode
      helpful
      highlight
      #highlight-escape-sequences
      #highlight-leading-spaces
      highlight-numbers
      highlight-symbol
      hydra
      #ialign
      ibuffer-vc
      iedit
      (with melpaPackages; [ imenu-list ])
      ivy
      ivy-hydra
      ivy-pass
      ivy-rich
      #jedi
      #jq-mode
      js-import
      js2-mode
      js2-refactor
      json-mode
      json-reformat
      json-snatcher
      (with melpaPackages; [
        key-chord
        lsp-haskell
        lsp-javascript-typescript
        lsp-go
        lsp-mode
        lsp-python
        lsp-rust
        lsp-ui
      ])
      magit
      magit-gitflow
      magit-popup
      markdown-mode
      (with melpaPackages; [ minions moody ])
      multiple-cursors
      (with melpaPackages; [
        nix-buffer
        nix-mode
        nix-sandbox
        nixos-options
      ])
      no-littering
      olivetti
      ob-go
      #ob-restclient
      ob-rust
      ob-typescript
      org-plus-contrib
      org-ref
      #org-super-agenda
      #org-web-tools
      (with melpaPackages; [ org-projectile ])
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
      # persistent-scratch
      pinentry
      popup
      projectile
      projectile-ripgrep
      (with melpaPackages; [ pulseaudio-control ])
      python-mode
      rainbow-delimiters
      rainbow-mode
      ripgrep
      (with melpaPackages; [ rjsx-mode ])
      shift-number
      shackle
      shx
      smart-jump
      (with melpaPackages; [ smartparens ])
      solaire-mode
      string-edit
      #tide
      toml-mode
      typescript-mode
      (with melpaPackages; [ undo-tree ])
      # (with melpaPackages; [ use-package use-package-chords use-package-ensure-system-package ])
      (with melpaPackages; [ use-package ])
      # vdiff
      visual-fill-column
      visual-regexp
      #visual-regexp-steroids
      #vlf
      web-mode
      wgrep
      which-key
      # window-purpose
      with-editor
      yasnippet
      yaml-mode
    ];
  };
}

