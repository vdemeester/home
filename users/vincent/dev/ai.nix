{ pkgs, ... }:

{
  home.packages = with pkgs; [
    aichat
    aider-chat
    python313Packages.google-generativeai
  ];
}
