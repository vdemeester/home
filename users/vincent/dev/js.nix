{ config, pkgs, ... }:

{
  # config.xdg.configFile."npm/npmrc ".text = ''
  #   prefix = ${config.home.homeDirectory}/.local/npm
  # '';
  home.sessionVariables = {
    NPM_CONFIG_INIT_MODULE = "${config.xdg.configHome}/npm/config/npm-init.js";
    NPM_CONFIG_CACHE = "${config.xdg.cacheHome}/npm";
    NPM_CONFIG_TMP = "$CONFIG.XDG_RUNTIME_DIR/npm";
    NPM_CONFIG_USERCONFIG = "${config.xdg.configHome}/npm/npmrc";
  };
  home.packages = with pkgs; [
    # javascript-typescript-langserver
    # vscode-langservers-extracted
  ];
}

