{ pkgs, ... }:
{
  imports = [
    ./atuin.nix
    ./direnv.nix
    ./fzf.nix
    ./git.nix
    ./htop.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  programs = {
    broot = {
      enable = true;
      enableZshIntegration = true;
    };
    eza.enable = true;
    fd.enable = true;
    git.enable = true;
    jq.enable = true;
  };

  home.packages = with pkgs; [
    age
    mosh
    ripgrep

    # ugrep
  ];
}
