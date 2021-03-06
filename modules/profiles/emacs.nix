{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.emacs;

  capture = pkgs.writeScriptBin "capture" ''
  #!${pkgs.stdenv.shell}
  emacsclient -n -F '((name . "capture") (width . 75) (height . 30))' -e '(org-capture)'
  '';
in
{
  options = {
    profiles.emacs = {
      enable = mkEnableOption "Enable emacs profile";
      capture = mkEnableOption "Enable capture script(s)";
      daemonService = mkOption {
        default = true;
        description = "Enable emacs daemon service";
        type = types.bool;
      };
      withXwidgets = mkEnableOption "Enable Xwidgets in emacs build";
      texlive = mkOption {
        default = true;
        description = "Enable Texlive";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.file.".local/share/applications/org-protocol.desktop".source = ./assets/xorg/org-protocol.desktop;
      home.file.".local/share/applications/ec.desktop".source = ./assets/xorg/ec.desktop;
      home.file.".local/share/applications/capture.desktop".source = ./assets/xorg/capture.desktop;
      home.packages = with pkgs; [
        ditaa
        graphviz
        pandoc
        zip
        hunspell
        hunspellDicts.en_US-large
        hunspellDicts.en_GB-ize
        hunspellDicts.fr-any
      ];
      home.sessionVariables = {
        EDITOR = "et";
      };
      programs.emacs = {
        enable = true;
        package = pkgs.emacs27.override { inherit (pkgs) imagemagick; withXwidgets = cfg.withXwidgets; };
        extraPackages = epkgs: with epkgs; [
          ace-window
          aggressive-indent
          async
          avy
          company
          company-emoji
          company-ghc
          company-go
          counsel
          counsel-projectile
          crux
          #dap-mode
          dash
          define-word
          delight
          dired-collapse
          dired-git-info
          dired-quick-sort
          dired-narrow
          dired-rsync
          direnv
          dockerfile-mode
          easy-kill
          exec-path-from-shell
          expand-region
          eyebrowse
          flycheck
          flycheck-golangci-lint
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
          hydra
          ibuffer-vc
          iedit
          ivy
          ivy-hydra
          ivy-prescient
          ivy-rich
          json-mode
          markdown-mode
          mpdel
          multiple-cursors
          no-littering
          ob-async
          ob-go
          ob-http
          # orca
          orgit
          org-plus-contrib
          org-capture-pop-frame
          org-gcal
          org-ref
          org-super-agenda
          org-web-tools
          # ox-epub
          ox-hugo
          ox-pandoc
          pandoc-mode
          pinentry
          # popup
          prescient
          projectile
          projectile-ripgrep
          pdf-tools
          python-mode
          rainbow-delimiters
          rainbow-mode
          region-bindings-mode
          ripgrep
          #smart-jump
          solaire-mode
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
          anzu
          company-lsp
          company-prescient
          darkroom
          eshell-prompt-extras
          esh-autosuggest
          fish-completion
          flyspell-correct-ivy
          forge
          go-mode
          hide-mode-line
          ivy-posframe
          lsp-mode
          lsp-ui
          magit
          magit-popup
          pretty-hydra
          major-mode-hydra
          minions
          moody
          mwim
          nix-buffer
          nix-mode
          org-super-agenda
          org-tree-slide
          shr-tag-pre-highlight
          smartparens
          symbol-overlay
          undo-tree
          use-package
          # Highly experimental
          emacs-libvterm
          gotest
        ];
      };
    }
    (mkIf config.profiles.emacs.capture {
      home.packages = with pkgs; [ wmctrl capture ];
    })
    (mkIf config.services.gpg-agent.enable {
      services.gpg-agent.extraConfig = ''
        allow-emacs-pinentry
      '';
    })
    (mkIf cfg.texlive {
      home.packages = with pkgs; [ texlive.combined.scheme-full ];
    })
    (mkIf cfg.daemonService {
      systemd.user.services.emacs = {
        Unit = {
          Description = "Emacs: the extensible, self-documenting text editor";
        };
        Service = {
          Environment = ''
          PATH=${config.home.homeDirectory}/.nix-profile/bin:${config.home.homeDirectory}/.local/npm/bin:/run/wrappers/bin:/etc/profiles/per-user/vincent/bin:${config.home.profileDirectory}/bin:/nix/var/nix/profiles/default/bin:/run/current-system/sw/bin:/usr/share/Modules/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:${config.home.homeDirectory}/bin GOPATH=${config.home.homeDirectory} NIX_PATH=${config.home.homeDirectory}/.nix-defexpr/channels:nixpkgs=/home/vincent/.nix-defexpr/channels/nixpkgs ASPELL_CONF=dict-dir=${config.home.homeDirectory}/.nix-profile/lib/aspell SSH_AUTH_SOCK=/run/user/1000/gnupg/S.gpg-agent.ssh NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt
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
