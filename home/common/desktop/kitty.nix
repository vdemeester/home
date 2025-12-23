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

    # Bépo-optimized tmux conditional bindings (Alt-based, context-aware)
    # These only activate when tmux is running (title contains "tmux")
    # Using Alt key to avoid conflicts with Niri window manager
    # Compatible with Emacs workflow (using Ctrl-B prefix instead of Ctrl-Space)
    extraConfig = ''
      # ============================================================================
      # BÉPO-OPTIMIZED KITTY TMUX BINDINGS (Alt-based, Emacs-compatible)
      # ============================================================================
      # Escape sequence: \x02 = Ctrl-B (default tmux prefix, Emacs-friendly)

      # Pane navigation - Alt+CTSR (bépo home row)
      map --when-focus-on title:tmux alt+c send_text all \x02c
      map --when-focus-on title:tmux alt+t send_text all \x02t
      map --when-focus-on title:tmux alt+s send_text all \x02s
      map --when-focus-on title:tmux alt+r send_text all \x02r

      # Pane resizing - Alt+Shift+CTSR
      map --when-focus-on title:tmux alt+shift+c send_text all \x02C
      map --when-focus-on title:tmux alt+shift+t send_text all \x02T
      map --when-focus-on title:tmux alt+shift+s send_text all \x02S
      map --when-focus-on title:tmux alt+shift+r send_text all \x02R

      # Window navigation - Alt+N/P (next/prev)
      map --when-focus-on title:tmux alt+n send_text all \x02p
      map --when-focus-on title:tmux alt+p send_text all \x02é

      # Move windows - Alt+Shift+N/P
      map --when-focus-on title:tmux alt+shift+n send_text all \x02P
      map --when-focus-on title:tmux alt+shift+p send_text all \x02É

      # Window/Pane creation & management
      map --when-focus-on title:tmux alt+enter send_text all \x02b
      map --when-focus-on title:tmux alt+w send_text all \x02w
      map --when-focus-on title:tmux alt+x send_text all \x02x

      # Splits
      map --when-focus-on title:tmux alt+minus send_text all \x02-
      map --when-focus-on title:tmux alt+backslash send_text all \x02|

      # Other actions
      map --when-focus-on title:tmux alt+z send_text all \x02z
      map --when-focus-on title:tmux alt+[ send_text all \x02[
      map --when-focus-on title:tmux alt+] send_text all \x02]
      map --when-focus-on title:tmux alt+a send_text all \x02a
      map --when-focus-on title:tmux alt+d send_text all \x02d
      map --when-focus-on title:tmux alt+shift+r send_text all \x02R
    '';

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
