{ config, pkgs, ... }:
{
  home.file."${config.programs.zsh.dotDir}/completion.zsh".source = ./zsh/completion.zsh;
  home.file."${config.programs.zsh.dotDir}/prompt.zsh".source = ./zsh/prompt.zsh;
  home.file."${config.programs.zsh.dotDir}/functions/j".source = ./zsh/j;
  home.file."${config.programs.zsh.dotDir}/auto-expanding-aliases.zsh".source =
    ./zsh/auto-expanding-aliases.zsh;

  home.packages = with pkgs; [
    nix-zsh-completions
  ];

  programs.zsh = {
    enable = true;
    # zprof.enable = true;
    # See https://gist.github.com/ctechols/ca1035271ad134841284
    completionInit = ''
      autoload -Uz compinit
      for dump in ${config.programs.zsh.dotDir}/.zcompdump(N.mh+24); do
        compinit
      done
      compinit -C
    '';
    enableCompletion = true;
    autosuggestion.enable = true;
    autocd = true;
    dotDir = "${config.xdg.configHome}/zsh";
    defaultKeymap = "emacs";
    history = {
      expireDuplicatesFirst = true;
      extended = true;
      ignoreDups = true;
      path = "${config.xdg.dataHome}/zsh_history";
      save = 10000;
      share = true;
    };
    envExtra = ''
      export PATH=$HOME/bin:$PATH
      export LESSHISTFILE="${config.xdg.dataHome}/less_history"
      export WEBKIT_DISABLE_COMPOSITING_MODE=1;
      if [ -d $HOME/.krew/bin ]; then
        export PATH=$HOME/.krew/bin:$PATH
      fi
      # TODO Move somewhere else
      export TLDR_CACHE_DIR="$XDG_CACHE_HOME"/tldr 
    '';
    # TODO Extract this to files.
    initExtra = ''
      # c.f. https://wiki.gnupg.org/AgentForwarding
      # gpgconf --create-socketdir &!
      path+="${config.programs.zsh.dotDir}/functions"
      fpath+="$HOME/.local/state/nix/profile/share/zsh/site-functions"
      fpath+="${config.programs.zsh.dotDir}/functions"
      for func (${config.programs.zsh.dotDir}/functions) autoload -U $func/*(x:t)
      autoload -Uz select-word-style; select-word-style bash
      #if [ -n "$INSIDE_EMACS" ]; then
      #  chpwd() { print -P "\033AnSiTc %d" }
      #  print -P "\033AnSiTu %n"
      #  print -P "\033AnSiTc %d"
      #fi
      if [[ "$TERM" == "dumb" || "$TERM" == "emacs" ]]
      then
        TERM=eterm-color
        unsetopt zle
        unsetopt prompt_cr
        unsetopt prompt_subst
        unfunction precmd
        unfunction preexec
        PS1='$ '
        return
      fi
      # eval "$(${config.programs.atuin.package}/bin/atuin init zsh)"
      # make sure navigation using emacs keybindings works on all non-alphanumerics
      # syntax highlighting
      source ${config.programs.zsh.dotDir}/plugins/zsh-nix-shell/nix-shell.plugin.zsh
      source ${pkgs.zsh-syntax-highlighting}/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh
      ZSH_HIGHLIGHT_PATTERNS+=('rm -rf *' 'fg=white,bold,bg=red')
      ZSH_HIGHLIGHT_PATTERNS+=('rm -fR *' 'fg=white,bold,bg=red')
      ZSH_HIGHLIGHT_PATTERNS+=('rm -fr *' 'fg=white,bold,bg=red')
      source ${config.programs.zsh.dotDir}/completion.zsh
      source ${config.programs.zsh.dotDir}/plugins/powerlevel10k/powerlevel10k.zsh-theme
      source ${config.programs.zsh.dotDir}/prompt.zsh
      source ${config.programs.zsh.dotDir}/plugins/kubectl-config-switcher/kubectl-config-switcher.plugin.zsh
      source ${config.programs.zsh.dotDir}/auto-expanding-aliases.zsh
      setopt HIST_IGNORE_SPACE
      alias -g L="|less"
      alias -g EEL=' 2>&1 | less'
      alias -g GB='`git rev-parse --abbrev-ref HEAD`'
      alias -g GR='`git rev-parse --show-toplevel`'
      alias -s {ape,avi,flv,m4a,mkv,mov,mp3,mp4,mpeg,mpg,ogg,ogm,wav,webm}=mpv
      alias -s org=emacs
      (( $+commands[jq] )) && alias -g MJ="| jq -C '.'"  || alias -g MJ="| ${pkgs.python3}/bin/python -mjson.tool"
      (( $+functions[zshz] )) && compdef _zshz j
      [[ -n $INSIDE_EMACS ]] && \
      function ff () {
        print "\e]51;Efind-file $(readlink -f $1)\e\\"
      }

      export _Z_DATA="${config.xdg.dataHome}/z"


      [ -n "$EAT_SHELL_INTEGRATION_DIR" ] && \
        source "$EAT_SHELL_INTEGRATION_DIR/zsh"
    '';
    loginExtra = ''
            # if [[ -z $DISPLAY && $TTY = /dev/tty1 ]]; then
            #   # exec dbus-run-session sway
      			# 	exec dbus-run-session niri-session
            # fi
    '';
    sessionVariables = {
      RPROMPT = "";
    };

    shellAliases = {
      mkdir = ''mkdir --parents --verbose'';
      rm = ''rm --interactive'';
      cp = ''cp --interactive'';
      mv = ''mv --interactive'';
      gcd = ''cd (git root)'';
      # ls = ''exa'';
      ll = ''ls -l'';
      la = ''ls -a'';
      l = ''ls -lah'';
      # t = ''exa --tree --level=2'';
      map = ''xargs -n1'';
      k = ''kubectl'';
      wget = ''wget -c --hsts-file=${config.xdg.dataHome}/wget-hsts'';
    };

    plugins = [
      {
        name = "kubectl-config-switcher";
        src = pkgs.fetchFromGitHub {
          owner = "chmouel";
          repo = "kubectl-config-switcher";
          rev = "5679aa70383cee93fc15351dd4895c29c90b78a5";
          sha256 = "sha256-Aifa5ms2p/l0FkZE8Tep8QiDWUdfFfdKrTIbJNurxw4=";
        };
      }
      {
        name = "zsh-z";
        src = pkgs.fetchFromGitHub {
          owner = "agkozak";
          repo = "zsh-z";
          rev = "aaafebcd97424c570ee247e2aeb3da30444299cd";
          sha256 = "sha256-9Wr4uZLk2CvINJilg4o72x0NEAl043lP30D3YnHk+ZA=";
        };
      }
      {
        name = "async";
        src = pkgs.fetchFromGitHub {
          owner = "mafredri";
          repo = "zsh-async";
          rev = "v1.8.5";
          sha256 = "sha256-mpXT3Hoz0ptVOgFMBCuJa0EPkqP4wZLvr81+1uHDlCc=";
        };
      }
      {
        name = "powerlevel10k";
        src = pkgs.fetchFromGitHub {
          owner = "romkatv";
          repo = "powerlevel10k";
          rev = "v1.20.0";
          sha256 = "sha256-ES5vJXHjAKw/VHjWs8Au/3R+/aotSbY7PWnWAMzCR8E=";
        };
      }
      {
        name = "zsh-nix-shell";
        src = pkgs.fetchFromGitHub {
          owner = "chisui";
          repo = "zsh-nix-shell";
          rev = "v0.8.0";
          sha256 = "sha256-Z6EYQdasvpl1P78poj9efnnLj7QQg13Me8x1Ryyw+dM=";
        };
      }
    ];
  };
}
