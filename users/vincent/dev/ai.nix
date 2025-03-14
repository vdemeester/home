{ pkgs, ... }:

{
  home.packages = with pkgs; [
    aichat
    aider-chat
    python312Packages.google-generativeai
    python313Packages.google-generativeai
    repomix
  ];
}
