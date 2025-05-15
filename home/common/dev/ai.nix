{ pkgs, ... }:
{
  home.packages = with pkgs; [
    aichat
    aider-chat
    goose-cli
    python312Packages.google-generativeai
    python313Packages.google-generativeai
    repomix
    # editors
    zed-editor
    code-cursor
    # mcp-servers
    github-mcp-server
  ];
}
