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
             name = "async";
             src = pkgs.fetchFromGitHub {
               owner = "mafredri";
               repo = "zsh-async";
               rev = "v1.7.0";
               sha256 = "1jbbypgn0r4pilhv2s2p11vbkkvlnf75wrhxfcvr7bfjpzyp9wbc";
             };
           }
          {
             name = "pure";
             src = pkgs.fetchFromGitHub {
               owner = "sindresorhus";
               repo = "pure";
               rev = "v1.10.3";
               sha256 = "0zjgnlw01ri0brx108n6miw4y0cxd6al1bh28m8v8ygshm94p1zx";
             };
           }
        ];
        initExtra = ''
          IS_SERIAL=0
          case $TTY in
            /dev/ttyS[0-9]*|/dev/ttyUSB[0-9]*)
            IS_SERIAL=1
            ;;
          esac
          IS_CHROOT=0
          if [[ $UID == 0 ]] && [[ $(stat -c %d:%i /) != $(stat -c %d:%i /proc/1/root/.) ]]; then
            IS_CHROOT=1
          fi
          PURE_PROMPT='λ'
          autoload -Uz select-word-style; select-word-style bash
          if [ -e /home/vincent/.nix-profile/etc/profile.d/nix.sh ]; then . /home/vincent/.nix-profile/etc/profile.d/nix.sh; fi
          # make sure navigation using emacs keybindings works on all non-alphanumerics
          # syntax highlighting
          source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
          ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
          ZSH_HIGHLIGHT_PATTERNS+=('rm -fR *' 'fg=white,bold,bg=red')
          ZSH_HIGHLIGHT_PATTERNS+=('rm -fr *' 'fg=white,bold,bg=red')
          # prompt
          autoload -Uz promptinit; promptinit
          PURE_CMD_MAX_EXEC_TIME=10
          zstyle :prompt:pure:path color white
          PURE_PROMPT_SYMBOL='λ'
          if (( IS_CHROOT )); then
            PURE_PROMPT_SYMBOL='(chroot) λ'
          fi
          if (( IS_SERIAL )); then
            # Serial can't handle beautiful symbols or setting the title ;).
            PURE_PROMPT_SYMBOL='>'
            prompt_pure_set_title() {}
          fi
          prompt pure
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
      programs.z-lua = {
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
