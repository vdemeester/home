{ pkgs, ... }:
{
  programs.aichat = {
    # enable = true;
    settings = {
      model = "gemini:gemini-2.5-flash-preview-04-17";
      wrap = 150;
      save_session = true;
      clients = [
        {
          type = "gemini";
          name = "gemini";
          api_base = "";
          # api_key = "passage:ai/gemini/api_key";
        }
      ];
    };
  };
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
