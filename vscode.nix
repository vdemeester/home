{ pkgs, ... }:

{
  home.packages = with pkgs; [
    vscode-with-extensions
  ];
}
