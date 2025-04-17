{ pkgs, ... }:
{
  programs.atuin = {
    enable = true;
    enableZshIntegration = true;
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
}
