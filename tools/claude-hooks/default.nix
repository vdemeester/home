{
  buildGoModule,
  lib,
  makeWrapper,
  libnotify,
}:

buildGoModule {
  pname = "claude-hooks";
  version = "0.1.0";
  src = ./.;

  vendorHash = "sha256-bdpAteulG3045jPdEpjcT4yGlnxLKDMlK7lk9WVRTKc=";

  nativeBuildInputs = [ makeWrapper ];

  # Build all binaries
  subPackages = [
    "cmd/capture-tool-output"
    "cmd/initialize-session"
    "cmd/update-terminal-title"
    "cmd/validate-docs"
    "cmd/save-session"
    "cmd/session-stats"
  ];

  # Rename binaries to have consistent prefix and wrap with dependencies
  postInstall = ''
    mv $out/bin/capture-tool-output $out/bin/claude-hooks-capture-tool-output
    mv $out/bin/initialize-session $out/bin/claude-hooks-initialize-session
    mv $out/bin/update-terminal-title $out/bin/claude-hooks-update-terminal-title
    mv $out/bin/validate-docs $out/bin/claude-hooks-validate-docs
    mv $out/bin/save-session $out/bin/claude-hooks-save-session
    mv $out/bin/session-stats $out/bin/claude-hooks-session-stats

    # Wrap save-session to include notify-send in PATH
    wrapProgram $out/bin/claude-hooks-save-session \
      --prefix PATH : ${lib.makeBinPath [ libnotify ]}
  '';

  meta = {
    description = "Claude Code hooks for session management, tool output capture, and documentation validation";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "claude-hooks-capture-tool-output";
  };
}
