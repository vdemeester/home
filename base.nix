{ pkgs, prefix, ...}:

let home_directory = builtins.getEnv "HOME"; in

rec {

  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/vdemeester/home-manager/archive/master.tar.gz;
    };
  };
  home.file.".nix-channels".source = ./nix-channels;
  home.file.".tmux.conf".text = ''
    source-file $HOME/.config/tmux/tmux.conf
    set-environment -g TMUX_PLUGIN_MANAGER_PATH '$HOME/.config/tmux/plugins'

    set -g @plugin 'tmux-plugins/tpm'
    set -g @plugin 'tmux-plugins/tmux-resurrect'
    set -g @plugin 'tmux-plugins/tmux-continuum'
    set -g @plugin 'tmux-plugins/tmux-copycat'

    set -g @continuum-restore 'on'

    run '${pkgs.tmux-tpm}/tpm'
'';
  xdg.configFile."tmux/tmux.conf".source = ./tmux/tmux.conf;
  xdg.configFile."tmux/commons/keybindings".source = ./tmux/keybindings;
  home.packages = with pkgs; [
    jq
    htop
    pass
    tree
    tmux
    enchive
  ];
}
