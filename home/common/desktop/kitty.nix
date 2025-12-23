{ pkgs, ... }:
{
  programs.kitty = {
    enable = true;
    shellIntegration.enableZshIntegration = true;
    settings = {
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
    # Automatic theme switching enabled via xdg.configFile below
    # Removed hardcoded themeFile to allow dark/light auto-switching
    # action_alias mkh kitten hints --alphabet asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM
    # map kitty_mod+n    mkh --type=linenum emacsclient -c -nw +{line} {path}
  };

  # Create automatic theme files for dark/light mode switching
  # Kitty will automatically use these based on GNOME's color-scheme setting
  xdg.configFile = {
    "kitty/dark-theme.auto.conf".source =
      "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Vivendi.conf";

    "kitty/light-theme.auto.conf".source =
      "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Operandi.conf";

    # Fallback for when no preference is set (use dark theme)
    "kitty/no-preference-theme.auto.conf".source =
      "${pkgs.kitty-themes}/share/kitty-themes/themes/Modus_Vivendi.conf";
  };

  programs.zsh.shellAliases = {
    ssh = ''kitty +kitten ssh'';
  };
}
