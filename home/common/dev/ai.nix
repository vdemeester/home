{ lib, pkgs, ... }:
{
  home.packages = with pkgs; [
    aichat
    aider-chat
    # goose-cli
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

  xdg.configFile."aichat/config.yaml.in".source = ./aichat.yaml;
  xdg.configFile."aichat/update-config" = {
    source = ./aichat-update-config;
    executable = true;
  };
  home.activation = {
    # linkGeneration writeBoundary
    aichat-configuration = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
      /home/vincent/.config/aichat/update-config
    '';
  };
}
