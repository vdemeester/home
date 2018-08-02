{ pkgs, prefix, ...}:

let home_directory = builtins.getEnv "HOME"; in

rec {

  imports = [ ../profiles/overlays.nix ];
  programs = {
    home-manager = {
      enable = true;
      path = https://github.com/vdemeester/home-manager/archive/fish-module.tar.gz;
    };
  };
  home.file.".tmux.conf".source = ./tmux/tmux.conf;
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
