{ pkgs, ... }:

{
  imports = [
    ./bash.nix
    ./direnv.nix
    ./fzf.nix
    ./git.nix
    ./gpg.nix
    ./htop.nix
    ./ssh.nix
    ./tmux.nix
    ./xdg.nix
    ./zsh.nix
  ];

  home = {
    stateVersion = "22.05";
    packages = with pkgs; [
      enchive
      entr
      # exa # TODO: switch to eza in 2024
      fd
      htop
      mosh
      ncurses
      pciutils
      ripgrep
      ugrep
      scripts
      tree
      broot
      lf
      usbutils
    ];
  };

  programs.atuin = {
    enable = true;
    flags = [ "--disable-up-arrow" ];
    settings = {
      auto_sync = true;
      sync_frequency = "10m";
      sync_address = "http://sakhalin.sbr.pm:8888";
      search_mode = "skim";
      filter_mode = "global";
      enter_accept = false;
      prefers_reduced_motion = true;
      common_prefix = [ "sudo" ];
      ignored_commands = [
        "cd"
        "fg"
        "ls"
        "rm"
        "vi"
        "vim"
        "p"
        "d"
        "ll"
        "j"
        "g"
        ":"
        "mv"
        "cat"
        "echo"
      ];
      # history_filter = [
      #   "^curl"
      #   "^wget"
      #   "^monolith"
      #   "^sherlock"
      #   "^yt-dlp"
      #   "^yt-dl"
      #   "^gallery-dl"
      #   "^archivebox"
      #   "^fanficfare"
      # ];
    };
  };

  # manpages are broken on 21.05 and home-manager (for some reason..)
  # (versionOlder nixosConfig.system.nixos.release "21.11");
  manual.manpages.enable = true;

  xdg.configFile."nixpkgs/config.nix".text = ''
    {
      allowUnfree = true;
    }
  '';
}
