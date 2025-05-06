{
  config,
  lib,
  pkgs,
  ...
}:
let
  shellConfig = import ./shell.nix { inherit config lib pkgs; };
in
{
  programs.bash = {
    enable = true;
    historyControl = [
      "erasedups"
      "ignorespace"
    ];
    historyFile = "${config.xdg.dataHome}/bash_history";
    historyFileSize = shellConfig.historySize;
    inherit (shellConfig) historySize;
    shellAliases = shellConfig.aliases;
  };
}
