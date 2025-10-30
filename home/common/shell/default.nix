{ pkgs, ... }:
{
  imports = [
    ./atuin.nix
    ./direnv.nix
    ./fzf.nix
    ./git.nix
    ./htop.nix
    ./openssh.nix
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

  programs.helix = {
    enable = true;
    settings = {
      theme = "modus_vivendi";
      editor.cursor-shape = {
        normal = "block";
        insert = "bar";
        select = "underline";
      };
    };
    languages.language = [
      {
        name = "nix";
        auto-format = true;
        formatter.command = "${pkgs.nixfmt}/bin/nixfmt";
      }
    ];
  };
  home.packages = with pkgs; [
    age
    mosh
    ripgrep

    shpool
    # ugrep
    scripts
    wol
  ];
}
