# Claude Code Hooks (Go Implementation)

Claude Code hooks implemented in Go for better performance, zero runtime dependencies, and native integration with the Nix ecosystem.

Migrated from TypeScript/Bun implementation in `dots/.claude/hooks/`.

## Hooks Included

### 1. `claude-hooks-initialize-session`
**Event**: SessionStart

**What it does**:
- Detects and skips subagent sessions
- Sets terminal tab title to "Claude Ready"
- Logs session start to `~/.config/claude/history/sessions/YYYY-MM/YYYY-MM-DD_session-log.txt`
- Implements debouncing to prevent duplicate triggers (2 second window)

### 2. `claude-hooks-capture-tool-output`
**Event**: PostToolUse

**What it does**:
- Captures outputs from interesting tools (Bash, Edit, Write, Read, Task, NotebookEdit, Skill, SlashCommand)
- Logs to JSONL files: `~/.config/claude/history/tool-outputs/YYYY-MM/YYYY-MM-DD_tool-outputs.jsonl`
- Silent failure - doesn't disrupt workflow if logging fails
- Automatically creates directory structure

### 3. `claude-hooks-validate-docs`
**Event**: Manual/Pre-commit

**What it does**:
- Scans all markdown files for internal links
- Checks if linked files exist
- Reports broken links with file:line numbers
- Exit code 0 if valid, 1 if broken links found
- Skips external URLs, anchors, and mailto links

### 4. `claude-hooks-save-session`
**Event**: SessionEnd

**What it does**:
- Prompts user to save session summary when ending a Claude Code session
- Skips subagent sessions (silent)
- Displays a message asking if session should be saved
- Works with `/save-session` slash command for creating session entries
- Saves to `~/.config/claude/history/sessions/YYYY-MM/YYYY-MM-DD-HHMMSS_SESSION_description.md`

## Architecture

```
tools/claude-hooks/
├── cmd/
│   ├── capture-tool-output/main.go   # PostToolUse hook
│   ├── initialize-session/main.go    # SessionStart hook
│   ├── save-session/main.go          # SessionEnd hook
│   └── validate-docs/main.go         # Documentation validator
├── internal/
│   └── paths/paths.go                # Shared path utilities
├── default.nix                       # Nix package definition
├── go.mod                            # Go module definition
├── go.sum                            # Go dependencies
├── setup-hooks.sh                    # Configuration script
└── README.md                         # This file
```

## Installation

### Via Nix (Recommended)

1. **Build the package**:
   ```bash
   nix build .#claude-hooks
   ```

2. **Install to your profile**:
   ```bash
   nix profile install .#claude-hooks
   ```

3. **Or add to home-manager** (in `home/common/dev/default.nix` or similar):
   ```nix
   home.packages = with pkgs; [
     claude-hooks
   ];
   ```

4. **Configure hooks** (run setup script):
   ```bash
   ./tools/claude-hooks/setup-hooks.sh
   ```

   Or manually update `~/.claude/settings.json`:
   ```json
   {
     "hooks": {
       "SessionStart": [
         {
           "hooks": [
             {
               "type": "command",
               "command": "claude-hooks-initialize-session"
             }
           ]
         }
       ],
       "PostToolUse": [
         {
           "hooks": [
             {
               "type": "command",
               "command": "claude-hooks-capture-tool-output"
             }
           ]
         }
       ],
       "SessionEnd": [
         {
           "hooks": [
             {
               "type": "command",
               "command": "claude-hooks-save-session"
             }
           ]
         }
       ]
     }
   }
   ```

5. **Enable the session-manager plugin** (for `/save-session` command):
   ```bash
   # Symlink the plugin if not already linked
   ln -s ~/src/home/dots/.claude/plugins/session-manager ~/.claude/plugins/session-manager
   ```

### Manual Build (without Nix)

```bash
cd tools/claude-hooks

# Build all hooks
go build -o bin/claude-hooks-capture-tool-output ./cmd/capture-tool-output
go build -o bin/claude-hooks-initialize-session ./cmd/initialize-session
go build -o bin/claude-hooks-save-session ./cmd/save-session
go build -o bin/claude-hooks-validate-docs ./cmd/validate-docs

# Copy to PATH
sudo cp bin/* /usr/local/bin/

# Then run setup script
./setup-hooks.sh
```

## Development

### Running hooks directly with `go run`

```bash
cd tools/claude-hooks

# Test initialize-session
go run ./cmd/initialize-session

# Test capture-tool-output (requires JSON input)
echo '{"tool_name":"Bash","tool_input":{},"tool_response":{},"conversation_id":"test"}' | \
  go run ./cmd/capture-tool-output

# Test validate-docs
go run ./cmd/validate-docs
```

### Testing

```bash
# Run all tests
go test ./...

# Test with verbose output
go test -v ./internal/paths

# Test specific package
go test ./cmd/validate-docs
```

### Updating vendorHash

If you add/update Go dependencies:

```bash
cd tools/claude-hooks
go mod tidy
nix build .#claude-hooks 2>&1 | grep "got:" | awk '{print $2}'
# Copy the hash to default.nix vendorHash field
```

## Advantages over TypeScript/Bun

1. **Zero runtime dependencies** - Compiled to native binary, no Bun required
2. **Faster execution** - Native code vs interpreted JavaScript
3. **Easier distribution** - Single binary per hook
4. **Cross-compilation** - Build for any architecture from any platform
5. **Nix integration** - Proper package management and reproducible builds
6. **Type safety** - Compile-time checks without runtime overhead
7. **Standard library** - Excellent built-in support for JSON, file I/O, paths

## Migration Notes

The Go implementation is functionally equivalent to the TypeScript version with these improvements:

- **Performance**: Faster startup and execution
- **Reliability**: Compile-time type checking
- **Portability**: Works on all systems where Go binaries run
- **Integration**: Native Nix packaging for declarative installation

The old TypeScript hooks in `dots/.claude/hooks/` can be removed after verifying the Go versions work correctly.

## Troubleshooting

**Hook not running:**
- Check `~/.claude/settings.json` syntax
- Verify binaries are in PATH: `which claude-hooks-initialize-session`
- Check stderr output in terminal
- Verify hooks are executable: `ls -la $(which claude-hooks-initialize-session)`

**Permission errors:**
- Ensure directories exist: `mkdir -p ~/.config/claude/history/{sessions,tool-outputs}`
- Check write permissions on `~/.config/claude/history`

**Debugging:**
- Hooks write errors to stderr - check terminal output
- Run hooks manually to test: `claude-hooks-initialize-session`
- Check log files: `ls -la ~/.config/claude/history/`

**Build errors:**
- Run `go mod tidy` to update dependencies
- Check Go version: `go version` (requires Go 1.23+)
- Verify vendorHash in `default.nix` is correct

## Environment Variables

- `CLAUDE_DIR` - Override default `~/.config/claude` directory (optional)
- `CLAUDE_PROJECT_DIR` - Set by Claude Code (used for subagent detection)
- `CLAUDE_AGENT_TYPE` - Set by Claude Code for subagents (used for detection)

## License

Same as the rest of the home repository.
