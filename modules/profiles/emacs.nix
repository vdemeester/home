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
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.file.".local/share/applications/org-protocol.desktop".source = ./assets/xorg/org-protocol.desktop;
      home.packages = with pkgs; [ pandoc rustracer ];
      programs.emacs = {
        enable = true;
        # package = pkgs.myEmacs;
        extraPackages = epkgs: with epkgs; [
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
          deft
          delight
          dired-collapse
          dired-sidebar
          direnv
          dockerfile-mode
          easy-kill
          eshell-bookmark
          (with melpaPackages; [
          eshell-prompt-extras
          esh-autosuggest
          ])
          exec-path-from-shell
          expand-region
          eyebrowse
          fish-mode
          (with melpaPackages; [ fish-completion ])
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
          (with melpaPackages; [
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
          ])
          hardhat
          helpful
          highlight
          #highlight-escape-sequences
          #highlight-leading-spaces
          highlight-numbers
          highlight-symbol
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
          (with melpaPackages; [
            key-chord
            lsp-haskell
            lsp-javascript-typescript
            lsp-go
            lsp-mode
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
            nix-update
            nixos-options
          ])
          no-littering
          ob-async
          ob-go
          ob-rust
          ob-typescript
          org-plus-contrib
          org-bullets
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
          ripgrep
          rust-mode
          scratch
          shift-number
          shackle
          shx
          smart-jump
          (with melpaPackages; [ smartparens ])
          solaire-mode
          string-edit
          toml-mode
          typescript-mode
          (with melpaPackages; [ undo-tree ])
          (with melpaPackages; [ use-package ])
          visual-fill-column
          visual-regexp
          web-mode
          wgrep
          which-key
          with-editor
          yasnippet
          yaml-mode
        ];
      };
    }
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
    })
  ]);
}
