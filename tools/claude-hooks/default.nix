{ buildGoModule }:

buildGoModule rec {
  pname = "claude-hooks";
  version = "0.1.0";
  src = ./.;

  vendorHash = "sha256-bdpAteulG3045jPdEpjcT4yGlnxLKDMlK7lk9WVRTKc=";

  # Build all three binaries
  subPackages = [
    "cmd/capture-tool-output"
    "cmd/initialize-session"
    "cmd/validate-docs"
  ];

  # Rename binaries to have consistent prefix
  postInstall = ''
    mv $out/bin/capture-tool-output $out/bin/claude-hooks-capture-tool-output
    mv $out/bin/initialize-session $out/bin/claude-hooks-initialize-session
    mv $out/bin/validate-docs $out/bin/claude-hooks-validate-docs
  '';

  meta = {
    description = "Claude Code hooks for session initialization, tool output capture, and documentation validation";
    mainProgram = "claude-hooks-capture-tool-output";
  };
}
