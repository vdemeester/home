{ pkgs, ... }:

{
  home.packages = with pkgs; [
    aichat
    aider-chat
    goose-cli
    ollama
    oterm
    python312Packages.google-generativeai
    python313Packages.google-generativeai
    repomix
  ];
}
