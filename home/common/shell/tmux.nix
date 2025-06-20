{ config, ... }:

{
  programs.tmux = {
    enable = true;

    clock24 = true;
    escapeTime = 0;
    newSession = true;

    terminal = "tmux-256color";

    extraConfig = ''
      source-file ${config.xdg.configHome}/tmux/tmux.conf
    '';
  };
  xdg.configFile."tmux/tmux.conf".source = ./tmux/tmux.conf;
}
