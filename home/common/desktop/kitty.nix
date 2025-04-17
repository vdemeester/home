_: {
  programs.kitty = {
    enable = true;
    shellIntegration.enableZshIntegration = true;
    settings = {
      term = "xterm-256color";
      close_on_child_death = "yes";
      font_family = "JetBrains Mono";
      tab_bar_edge = "top";
      listen_on = "unix:/tmp/my kitty";
      allow_remote_control = "yes";
      macos_option_as_alt = "yes";
      copy_on_select = "yes";
      notify_on_cmd_finish = "invisible 5.0 notify";

      active_tab_foreground = "#C6A0F6";
      active_tab_background = "#0c0c0c";
      inactive_tab_foreground = "#6E738D";
      inactive_tab_background = "#0c0c0c";
    };
    font = {
      name = "JetBrains Mono";
      size = 12;
    };
    keybindings = {
      "shift+left" = "neighboring_window left";
      "shift+right" = "neighboring_window right";
      "shift+up" = "neighboring_window up";
      "shift+down" = "neighboring_window down";
    };
    theme = "Tango Light";
    # action_alias mkh kitten hints --alphabet asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM 
    # map kitty_mod+n    mkh --type=linenum emacsclient -c -nw +{line} {path}
  };
}
