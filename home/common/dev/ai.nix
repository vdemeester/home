{
  inputs,
  pkgs,
  ...
}:
{
  home.packages = with pkgs; [
    chmouzies-ai
    aichat
    aider-chat
    goose-cli
    gemini-cli
    claude-code
    # llm
    # openai-whisper
    # whisper-cpp
    # python312Packages.google-generativeai
    # python313Packages.google-generativeai
    repomix
    # editors
    code-cursor
    cursor-cli
    # inputs.code-cursor-nix.packages.x86_64-linux.cursor
    # mcp-servers
    github-mcp-server
    inputs.copilot-cli.packages.x86_64-linux.default
    # amp-cli
  ];

  xdg.configFile."aichat/config.yaml.in".source = ./aichat.yaml;
  xdg.configFile."aichat/models-override.yaml".source = ./aichat-models-override.yaml;
  xdg.configFile."aichat/genconf.py" = {
    source = ./genconf.py;
    executable = true;
  };
  # xdg.configFile."aichat/update-config" = {
  #   source = ./aichat-update-config;
  #   executable = true;
  # };
  # home.activation = {
  #   # linkGeneration writeBoundary
  #   aichat-configuration = lib.hm.dag.entryAfter [ "linkGeneration" ] ''
  #     /home/vincent/.config/aichat/genconf.py
  #   '';
  # };
}
