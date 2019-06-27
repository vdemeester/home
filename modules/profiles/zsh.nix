{ config, lib, pkgs, ... }:

with lib;
let
  cfg = config.profiles.zsh;
in
{
  options = {
    profiles.zsh = {
      enable = mkOption {
        default = false;
        description = "Enable zsh profile and configuration";
        type = types.bool;
      };
    };
  };
  config = mkIf cfg.enable (mkMerge [
    {
      home.packages = with pkgs; [
        zsh-syntax-highlighting
      ];
      programs.zsh = {
        enable = true;
        dotDir = ".config/zsh";
        autocd = true;
        defaultKeymap = "emacs";
        enableAutosuggestions = true;
        history = {
          size = 100000;
          expireDuplicatesFirst = true;
          ignoreDups = true;
        };
        localVariables = {
          EMOJI_CLI_KEYBIND = "^n";
          EMOJI_CLI_USE_EMOJI = "yes";
          ZSH_HIGHLIGHT_HIGHLIGHTERS = [ "main" "brackets" "pattern" ];
        };
        shellAliases = import ./aliases.shell.nix;
        plugins = [
          {
             name = "emoji-cli";
             src = pkgs.fetchFromGitHub {
               owner = "b4b4r07";
               repo = "emoji-cli";
               rev = "26e2d67d566bfcc741891c8e063a00e0674abc92";
               sha256 = "0n88w4k5vaz1iyikpmlzdrrkxmfn91x5s4q405k1fxargr1w6bmx";
             };
           }
          {
             name = "zsh-history-substring-search";
             src = pkgs.fetchFromGitHub {
               owner = "zsh-users";
               repo = "zsh-history-substring-search";
               rev = "0f80b8eb3368b46e5e573c1d91ae69eb095db3fb";
               sha256 = "0y8va5kc2ram38hbk2cibkk64ffrabfv1sh4xm7pjspsba9n5p1y";
             };
           }
        ];
        initExtra = ''
          if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
          # make sure navigation using emacs keybindings works on all non-alphanumerics
          autoload -U select-word-style
          select-word-style bash
          # syntax highlighting
          source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
          ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
          ZSH_HIGHLIGHT_PATTERNS+=('rm -fR *' 'fg=white,bold,bg=red')
          ZSH_HIGHLIGHT_PATTERNS+=('rm -fr *' 'fg=white,bold,bg=red')
          # history-substring-search
          bindkey '^[[A' history-substring-search-up
          bindkey '^[[B' history-substring-search-down
        '';
        profileExtra = ''
          if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
          export NIX_PATH=$HOME/.nix-defexpr/channels:$NIX_PATH
        '';
      };
      programs.fzf = {
        enable = true;
        enableZshIntegration = true;
      };
    }
    (mkIf config.profiles.emacs.enable {
      /*programs.zsh.initExtra = ''
        export EDITOR=et
      '';*/
    })
    ]);
}
