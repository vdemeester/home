{ config, pkgs, ... }:

{
  xdg.configFile."python/pythonrc".source = ./python/pythonrc;
  home.packages = with pkgs; [
    pipenv
    python313
    python313Packages.python-lsp-server
    uv
  ];
  home.sessionVariables = {
    PYTHONSTARTUP = "${config.xdg.configHome}/python/pythonrc";
  };
}
