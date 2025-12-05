{
  buildGoModule,
  lib,
}:

buildGoModule {
  pname = "claude-hooks";
  version = "0.1.0";
  src = ./.;

  vendorHash = "sha256-bdpAteulG3045jPdEpjcT4yGlnxLKDMlK7lk9WVRTKc=";

  # Build all binaries
  subPackages = [
    "cmd/capture-tool-output"
    "cmd/initialize-session"
    "cmd/validate-docs"
    "cmd/save-session"
  ];

  # Rename binaries to have consistent prefix
  postInstall = ''
    mv $out/bin/capture-tool-output $out/bin/claude-hooks-capture-tool-output
    mv $out/bin/initialize-session $out/bin/claude-hooks-initialize-session
    mv $out/bin/validate-docs $out/bin/claude-hooks-validate-docs
    mv $out/bin/save-session $out/bin/claude-hooks-save-session
  '';

  meta = {
    description = "Claude Code hooks for session management, tool output capture, and documentation validation";
    license = lib.licenses.mit;
    platforms = lib.platforms.unix;
    mainProgram = "claude-hooks-capture-tool-output";
  };
}
