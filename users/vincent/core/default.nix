{ pkgs, ... }:

{
  imports = [
    ./bash.nix
    ./direnv.nix
    ./fzf.nix
    ./git.nix
    ./gpg.nix
    ./htop.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  home = {
    stateVersion = "20.03";
    packages = with pkgs; [
      enchive
      entr
      exa
      fd
      htop
      mpw
      mosh
      ncurses
      ripgrep
      scripts
      tree
    ];
  };

  xdg.configFile."nixpkgs/config.nix".text = "{ allowUnfree = true; }";
}
