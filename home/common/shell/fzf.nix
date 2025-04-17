_: {
  programs.fzf = {
    enable = true;
    enableZshIntegration = true;
    defaultOptions = [ "--bind=ctrl-j:accept" ];
    changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
    fileWidgetCommand = "rg --files";
    fileWidgetOptions = [
      "--preview 'bat -n --color=always {}'"
      "--bind 'ctrl-/:change-preview-window(down|hidden|)'"
    ];
  };
}
