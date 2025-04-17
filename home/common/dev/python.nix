{ config, pkgs, ... }:
{
  xdg.configFile."python/pythonrc".source = ./python/pythonrc;
  home.packages = with pkgs; [
    (pkgs.python313.withPackages (
      p: with p; [
        tox
        virtualenv
        python-lsp-server
      ]
    ))
    uv
    ruff
  ];
  home.sessionVariables = {
    PYTHONSTARTUP = "${config.xdg.configHome}/python/pythonrc";
  };
}
