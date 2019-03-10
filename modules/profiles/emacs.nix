{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.emacs;
in
{
  options = {
    profiles.emacs = {
      enable = mkOption {
        default = false;
        description = "Enable emacs profile";
        type = types.bool;
      };
      daemonService = mkOption {
        default = true;
        description = "Enable emacs daemon service";
        type = types.bool;
      };
      withXwidgets = mkOption {
        default = false;
        description = "Enable Xwidgets in emacs build";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.file.".local/share/applications/org-protocol.desktop".source = ./assets/xorg/org-protocol.desktop;
      home.packages = with pkgs; [ pandoc rustracer wmctrl ];
      programs.emacs = {
        enable = true;
        package = pkgs.emacs.override { inherit (pkgs) imagemagick; withXwidgets = cfg.withXwidgets; };
        extraPackages = epkgs: with epkgs; [
          ace-link
          ace-window
          aggressive-indent
          async
          avy
          bm
          command-log-mode
          company
          company-emoji
          company-ghc
          company-go
          company-lsp
          company-racer
          counsel
          counsel-projectile
          dash
          define-word
          deft
          delight
          dired-collapse
          dired-sidebar
          direnv
          dockerfile-mode
          easy-kill
          eshell-bookmark
          exec-path-from-shell
          expand-region
          eyebrowse
          fish-mode
          flycheck
          flycheck-golangci-lint
          flycheck-popup-tip
          flycheck-rust
          fold-this
          fullframe
          git-commit
          gitattributes-mode
          gitconfig-mode
          gitignore-mode
          hardhat
          helpful
          highlight
          #highlight-escape-sequences
          #highlight-leading-spaces
          highlight-numbers
          #highlight-symbol
          hydra
          ibuffer-vc
          iedit
          ivy
          ivy-hydra
          ivy-rich
          js-import
          js2-mode
          js2-refactor
          json-mode
          ledger-mode
          hledger-mode
          ledger-import
          magit
          magit-gitflow
          magit-popup
          markdown-mode
          multiple-cursors
          no-littering
          notmuch
          ob-async
          ob-go
          ob-http
          ob-rust
          ob-typescript
          orca
          org-plus-contrib
          org-bullets
          org-capture-pop-frame
          org-ref
          org-super-agenda
          org-web-tools
          ox-epub
          ox-hugo
          ox-ioslide
          ox-pandoc
          ox-slack
          ox-tufte
          ox-twbs
          pandoc-mode
          pinentry
          popup
          projectile
          projectile-ripgrep
          pdf-tools
          python-mode
          racer
          rainbow-delimiters
          rainbow-mode
          region-bindings-mode
          request
          request-deferred
          ripgrep
          rust-mode
          scratch
          shackle
          shx
          smart-jump
          solaire-mode
          string-edit
          toml-mode
          typescript-mode
          try
          visual-fill-column
          visual-regexp
          web-mode
          wgrep
          which-key
          with-editor
          xterm-color
          yasnippet
          yaml-mode
        ] ++ (with melpaPackages; [
          eshell-prompt-extras
          esh-autosuggest
          fish-completion
          godoctor
          go-add-tags
          go-eldoc
          go-errcheck
          go-fill-struct
          go-guru
          # go-impl
          go-mode
          #gorepl-mode
          go-tag
          gotest
          key-chord
          lsp-haskell
          lsp-javascript-typescript
          lsp-go
          lsp-mode
          lsp-rust
          lsp-ui
          #(with melpaPackages; [ pretty-hydra major-mode-hydra ])
          minions
          moody
          nix-buffer
          nix-mode
          nix-sandbox
          nix-update
          nixos-options
          smartparens
          symbol-overlay
          undo-tree
          use-package
        ]);
      };
    }
    (mkIf config.profiles.desktop.enable {
      home.file."bin/capture" = {
        source = ./assets/bin/capture;
        executable = true;
      };
    })
    (mkIf config.services.gpg-agent.enable {
      services.gpg-agent.extraConfig = ''
        allow-emacs-pinentry
      '';
    })
    (mkIf cfg.daemonService {
      systemd.user.services.emacs = {
        Unit = {
          Description = "Emacs: the extensible, self-documenting text editor";
        };
        Service = {
          Environment = ''
            PATH=${config.home.homeDirectory}/bin:${config.home.homeDirectory}/.local/npm/bin:/run/wrappers/bin:/etc/profiles/per-user/vincent/bin:${config.home.profileDirectory}/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin GOPATH=${config.home.homeDirectory} ASPELL_CONF=dict-dir=${config.home.homeDirectory}/.nix-profile/lib/aspell
          '';
          Type      = "forking";
          ExecStart = "${pkgs.bash}/bin/bash -c 'source /etc/profile; exec ${config.home.homeDirectory}/.nix-profile/bin/emacs --daemon'";
          ExecStop  = "${config.home.homeDirectory}/.nix-profile/bin/emacsclient --eval (kill-emacs)";
          Restart   = "always";
        };
        Install = {
          WantedBy = [ "default.target" ];
        };
      };
    })
  ]);
}
