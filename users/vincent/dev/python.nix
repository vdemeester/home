{ pkgs, ... }:

{
  home.packages = with pkgs; [
    python3
    pipenv
    python-lsp-server
  ];
}
