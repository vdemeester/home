{ pkgs, ... }:
{
  home.packages = with pkgs; [
    aichat
    aider-chat
    goose-cli
    llm
    # openai-whisper
    whisper-cpp
    python312Packages.google-generativeai
    python313Packages.google-generativeai
    repomix
    # editors
    zed-editor
    code-cursor
    # mcp-servers
    github-mcp-server
    amp
  ];
}
