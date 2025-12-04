# Hook System

**Event-Driven Automation Infrastructure**

**Location:** `/home/vincent/src/home/tools/claude-hooks/`
**Configuration:** `/home/vincent/.config/claude/settings.json`
**Status:** Active - Go-based implementation

---

## Overview

The hook system is an event-driven automation infrastructure built on Claude Code's native hook support. Hooks are executable commands (Go binaries) that run automatically in response to specific events during Claude Code sessions.

**Core Capabilities:**
- **Session Management** - Set terminal titles, log session starts
- **Tool Output Capture** - Automatic logging of tool executions to JSONL files
- **Session Prompts** - Prompt to save session summaries on exit
- **Subagent Detection** - Skip hooks for subagent sessions

**Key Principle:** Hooks run asynchronously and fail gracefully. They enhance the user experience but never block Claude Code's core functionality.

---

## Architecture

```
/home/vincent/src/home/tools/claude-hooks/
├── cmd/
│   ├── initialize-session/main.go     # SessionStart hook
│   ├── capture-tool-output/main.go    # PostToolUse hook
│   ├── save-session/main.go           # SessionEnd hook
│   └── validate-docs/main.go          # Documentation validator (not used as hook)
├── internal/
│   └── paths/paths.go                 # Shared path utilities
├── default.nix                        # Nix package definition
├── go.mod                             # Go module definition
├── go.sum                             # Go dependencies
├── setup-hooks.sh                     # Configuration script
└── README.md                          # Documentation
```

**Implementation:** Go binaries compiled to native code for zero runtime dependencies, faster execution, and cross-platform compatibility.

**Migration:** Migrated from TypeScript/Bun implementation previously in `dots/.claude/hooks/`.

---

## Available Hook Types (Configured)

You currently have 3 hooks configured. Claude Code supports 8 hook event types total, but only these 3 are in use.

### 1. **SessionStart**
**When:** Claude Code session begins (new conversation)
**Command:** `claude-hooks-initialize-session`

**Current Configuration:**
```json
{
  "SessionStart": [
    {
      "hooks": [
        {
          "type": "command",
          "command": "claude-hooks-initialize-session"
        }
      ]
    }
  ]
}
```

**What It Does:**
- **Detects subagent sessions**: Skips if `CLAUDE_AGENT_TYPE` is set or path contains `/.claude/agents/`
- **Sets terminal tab title**: Sets tab title to "Claude Ready" using ANSI escape codes
- **Logs session start**: Appends to `~/.config/claude/history/sessions/YYYY-MM/YYYY-MM-DD_session-log.txt`
- **Implements debouncing**: 2-second window to prevent duplicate triggers

**Implementation Details:**
```go
// From initialize-session/main.go:88-113
func main() {
    // Check if this is a subagent session
    if isSubagentSession() {
        fmt.Fprintln(os.Stderr, "🤖 Subagent session detected - skipping session initialization")
        os.Exit(0)
    }

    // Check debounce to prevent duplicate notifications
    if shouldDebounce() {
        fmt.Fprintln(os.Stderr, "🔇 Debouncing duplicate SessionStart event")
        os.Exit(0)
    }

    // Set initial tab title
    tabTitle := "Claude Ready"
    setTerminalTitle(tabTitle)
    fmt.Fprintf(os.Stderr, "📍 Session initialized: \"%s\"\n", tabTitle)

    // Log session start to history (silent failure)
    if err := logSessionStart(); err != nil {
        // Don't break session start for logging issues
        fmt.Fprintf(os.Stderr, "[initialize-session] Warning: Could not log session start: %v\n", err)
    }

    os.Exit(0)
}
```

**Subagent Detection:**
Checks environment variables to determine if this is a subagent session:
- `CLAUDE_PROJECT_DIR` contains `/.claude/agents/`
- `CLAUDE_AGENT_TYPE` is set

**Debouncing:**
Uses a lockfile in `/tmp/claude-session-start.lock` with timestamps to prevent duplicate notifications within 2 seconds.

---

### 2. **PostToolUse**
**When:** After Claude executes any tool
**Command:** `claude-hooks-capture-tool-output`

**Current Configuration:**
```json
{
  "PostToolUse": [
    {
      "hooks": [
        {
          "type": "command",
          "command": "claude-hooks-capture-tool-output"
        }
      ]
    }
  ]
}
```

**What It Does:**
- **Captures tool outputs**: Logs selected tool executions to JSONL files
- **Filters by tool type**: Only captures these tools:
  - `Bash`
  - `Edit`
  - `Write`
  - `Read`
  - `Task`
  - `NotebookEdit`
  - `Skill`
  - `SlashCommand`
- **Logs to daily files**: `~/.config/claude/history/tool-outputs/YYYY-MM/YYYY-MM-DD_tool-outputs.jsonl`
- **Silent failure**: Doesn't disrupt workflow if logging fails
- **Auto-creates directories**: Creates year-month subdirectories as needed

**JSONL Format:**
```json
{
  "timestamp": "2024-01-15T10:30:00Z",
  "tool": "Bash",
  "input": {"command": "ls -la"},
  "output": {"stdout": "..."},
  "session": "conversation-id-abc123"
}
```

**Implementation Details:**
```go
// From capture-tool-output/main.go:32-42
var interestingTools = map[string]bool{
    "Bash":         true,
    "Edit":         true,
    "Write":        true,
    "Read":         true,
    "Task":         true,
    "NotebookEdit": true,
    "Skill":        true,
    "SlashCommand": true,
}

// Only capture interesting tools
if !interestingTools[data.ToolName] {
    os.Exit(0)
}
```

**Hook Input (stdin):**
Claude Code sends JSON data on stdin:
```json
{
  "tool_name": "Bash",
  "tool_input": {"command": "ls -la"},
  "tool_response": {"stdout": "..."},
  "conversation_id": "session-id",
  "timestamp": "2024-01-15T10:30:00Z"
}
```

---

### 3. **SessionEnd**
**When:** Claude Code session terminates (conversation ends)
**Command:** `claude-hooks-save-session`

**Current Configuration:**
```json
{
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
```

**What It Does:**
- **Prompts user to save session**: Displays message asking if session should be saved
- **Skips subagent sessions**: Silent exit for subagent sessions
- **Works with `/save-session` command**: Integrates with session-manager plugin
- **Documents session**: Creates summary in `~/.config/claude/history/sessions/YYYY-MM/YYYY-MM-DD-HHMMSS_SESSION_description.md`

**Prompt Output:**
```
---

**Session ending**. Would you like me to save a summary of this session to your history?

I can create a session entry in `~/.config/claude/history/sessions/` documenting:
- What was accomplished
- Decisions made
- Next steps
- Related notes
```

**Implementation Details:**
```go
// From save-session/main.go:20-42
func main() {
    // Check if this is a subagent session
    if isSubagentSession() {
        // Silent exit for subagent sessions
        os.Exit(0)
    }

    // Output prompt for Claude to save the session
    fmt.Println("")
    fmt.Println("---")
    fmt.Println("")
    fmt.Println("**Session ending**. Would you like me to save a summary of this session to your history?")
    // ... rest of prompt
}
```

---

## Other Hook Events (Not Configured)

Claude Code supports these additional hook events that are **not currently configured** in your setup:

### **UserPromptSubmit**
**When:** User submits a new prompt to Claude
**Use Cases:** Update UI indicators, pre-process user input, capture prompts

### **Stop**
**When:** Main agent completes a response
**Use Cases:** Voice notifications, capture work summaries, update terminal tabs

### **SubagentStop**
**When:** Subagent (Task tool) completes execution
**Use Cases:** Agent-specific notifications, capture agent outputs

### **PreToolUse**
**When:** Before Claude executes any tool
**Use Cases:** Tool usage analytics, pre-execution validation

### **PreCompact**
**When:** Before Claude compacts context (long conversations)
**Use Cases:** Preserve important context, log compaction events

To configure these events, add them to `/home/vincent/.config/claude/settings.json` in the `"hooks"` section. You would need to create corresponding Go implementations in the `claude-hooks` project.

---

## Configuration

### Location
**File:** `/home/vincent/.config/claude/settings.json`
**Section:** `"hooks": { ... }`

### Environment Variables
Hooks have access to environment variables:

**Claude Code Variables:**
- `CLAUDE_PROJECT_DIR` - Project directory (used for subagent detection)
- `CLAUDE_AGENT_TYPE` - Agent type for subagents (used for detection)

**Custom Variables (optional):**
- `CLAUDE_DIR` - Override default `~/.config/claude` directory

### Hook Configuration Structure

```json
{
  "hooks": {
    "HookEventName": [
      {
        "matcher": "pattern",  // Optional: filter which tools/events trigger hook
        "hooks": [
          {
            "type": "command",
            "command": "command-to-execute"
          }
        ]
      }
    ]
  }
}
```

**Fields:**
- `HookEventName` - One of: SessionStart, SessionEnd, UserPromptSubmit, Stop, SubagentStop, PreToolUse, PostToolUse, PreCompact
- `matcher` - Pattern to match (use `"*"` for all tools, or specific tool names) - only relevant for tool-based events
- `type` - Always `"command"` (executes external command)
- `command` - Name of executable command (must be in PATH)

### Hook Input (stdin)
All hooks receive JSON data on stdin. The format varies by event type.

**Example for PostToolUse:**
```json
{
  "tool_name": "Bash",
  "tool_input": {"command": "ls -la"},
  "tool_response": {"stdout": "..."},
  "conversation_id": "session-id",
  "timestamp": "2024-01-15T10:30:00Z"
}
```

---

## Common Patterns

### 1. Silent Failure

**Pattern:** Wrap everything in error handling → Log errors → Always exit successfully

```go
func main() {
    // Read stdin
    input, err := io.ReadAll(os.Stdin)
    if err != nil {
        fmt.Fprintf(os.Stderr, "[hook-name] Error reading stdin: %v\n", err)
        os.Exit(0) // Exit 0 - don't break Claude Code
    }

    // Hook logic here
    if err := doWork(); err != nil {
        fmt.Fprintf(os.Stderr, "[hook-name] Error: %v\n", err)
        os.Exit(0) // Always exit 0
    }

    os.Exit(0)
}
```

**Why:** If hooks crash (exit 1), Claude Code may freeze. Always exit cleanly with code 0.

---

### 2. Subagent Detection

**Pattern:** Check environment variables → Skip hook if subagent

```go
func isSubagentSession() bool {
    claudeProjectDir := os.Getenv("CLAUDE_PROJECT_DIR")
    if strings.Contains(claudeProjectDir, "/.claude/agents/") {
        return true
    }
    if os.Getenv("CLAUDE_AGENT_TYPE") != "" {
        return true
    }
    return false
}

func main() {
    if isSubagentSession() {
        fmt.Fprintln(os.Stderr, "🤖 Subagent session - skipping")
        os.Exit(0)
    }
    // Main hook logic
}
```

**Why:** Subagent sessions (Task tool) often don't need the same initialization or prompts as main sessions.

---

### 3. History Capture

**Pattern:** Parse data → Save to appropriate history directory

**File Naming Convention:**
```
YYYY-MM-DD-HHMMSS_TYPE_description.md
```

**Directory Structure:**
```
~/.config/claude/history/
├── sessions/
│   └── 2024-01/
│       ├── 2024-01-15_session-log.txt
│       └── 2024-01-15-103000_SESSION_kubernetes-skill.md
└── tool-outputs/
    └── 2024-01/
        └── 2024-01-15_tool-outputs.jsonl
```

**Example:**
```go
yearMonth := time.Now().Format("2006-01")
dateDir := filepath.Join(paths.HistoryDir(), "sessions", yearMonth)
os.MkdirAll(dateDir, 0755)

logFile := filepath.Join(dateDir, fmt.Sprintf("%s_session-log.txt", today))
f, _ := os.OpenFile(logFile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
f.WriteString(logEntry)
```

---

### 4. JSONL Logging

**Pattern:** Append JSON entries to daily log files

```go
entry := CaptureEntry{
    Timestamp: time.Now().Format(time.RFC3339),
    Tool:      "Bash",
    Input:     toolInput,
    Output:    toolOutput,
    Session:   sessionID,
}

jsonData, _ := json.Marshal(entry)
f.WriteString(string(jsonData) + "\n")
```

**Why:** JSONL (JSON Lines) format allows streaming appends and easy parsing with tools like `jq`.

**Query Examples:**
```bash
# Count tool uses by type
jq -r '.tool' ~/.config/claude/history/tool-outputs/2024-01/2024-01-15_tool-outputs.jsonl | sort | uniq -c

# Extract all Bash commands
jq -r 'select(.tool=="Bash") | .input.command' ~/.config/claude/history/tool-outputs/2024-01/*.jsonl

# Find tools from specific session
jq -r 'select(.session=="abc123")' ~/.config/claude/history/tool-outputs/2024-01/*.jsonl
```

---

### 5. Terminal Title Setting

**Pattern:** Use ANSI escape codes to update terminal tab title

```go
func setTerminalTitle(title string) {
    fmt.Fprintf(os.Stderr, "\x1b]0;%s\x07", title)  // Standard
    fmt.Fprintf(os.Stderr, "\x1b]2;%s\x07", title)  // iTerm2
    fmt.Fprintf(os.Stderr, "\x1b]30;%s\x07", title) // Kitty
}
```

**Why:** Helps identify Claude Code sessions in terminal multiplexers (tmux, kitty, etc.)

**ANSI Escape Codes:**
- `\x1b]0;...\x07` - OSC 0 (Icon name and window title)
- `\x1b]2;...\x07` - OSC 2 (Window title)
- `\x1b]30;...\x07` - OSC 30 (Kitty tab title)

---

### 6. Debouncing

**Pattern:** Use lockfiles with timestamps to prevent duplicate triggers

```go
const debounceDuration = 2 * time.Second

func shouldDebounce() bool {
    lockfile := filepath.Join(os.TempDir(), "claude-session-start.lock")

    data, err := os.ReadFile(lockfile)
    if err == nil {
        lockTime, _ := strconv.ParseInt(string(data), 10, 64)
        now := time.Now().UnixMilli()
        if now-lockTime < debounceDuration.Milliseconds() {
            return true // Within debounce window
        }
    }

    // Update lockfile
    now := time.Now().UnixMilli()
    os.WriteFile(lockfile, []byte(fmt.Sprintf("%d", now)), 0644)
    return false
}
```

**Why:** Claude Code may trigger hooks multiple times in rapid succession. Debouncing prevents duplicate notifications.

---

## Installation and Setup

### Building from Source (Nix)

```bash
cd /home/vincent/src/home

# Build all hooks
nix build .#claude-hooks

# Install to user profile
nix profile install .#claude-hooks
```

### Adding to Home Manager

In your `home.nix` or equivalent:
```nix
home.packages = with pkgs; [
  claude-hooks
];
```

### Manual Setup

```bash
cd /home/vincent/src/home/tools/claude-hooks

# Build all hooks
go build -o bin/claude-hooks-initialize-session ./cmd/initialize-session
go build -o bin/claude-hooks-capture-tool-output ./cmd/capture-tool-output
go build -o bin/claude-hooks-save-session ./cmd/save-session

# Copy to PATH
sudo cp bin/* /usr/local/bin/

# Run setup script (configures settings.json)
./setup-hooks.sh
```

### Verify Installation

```bash
# Check binaries are in PATH
which claude-hooks-initialize-session
which claude-hooks-capture-tool-output
which claude-hooks-save-session

# Test hooks manually
claude-hooks-initialize-session

# Test with input
echo '{"tool_name":"Bash","tool_input":{},"tool_response":{},"conversation_id":"test"}' | \
  claude-hooks-capture-tool-output
```

### Configuration

Edit `/home/vincent/.config/claude/settings.json` to enable hooks (should already be configured):

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

Restart Claude Code after editing settings.json.

---

## Creating Custom Hooks

### Step 1: Choose Hook Event
Decide which event should trigger your hook (SessionStart, PostToolUse, etc.)

### Step 2: Create Hook Command

**Option A: Go Implementation** (recommended)

Create new command in `/home/vincent/src/home/tools/claude-hooks/cmd/`:

```go
package main

import (
    "encoding/json"
    "fmt"
    "io"
    "os"
)

type HookInput struct {
    // Define fields based on hook event type
    ConversationID string `json:"conversation_id"`
    // ... other fields
}

func main() {
    // Read stdin
    input, err := io.ReadAll(os.Stdin)
    if err != nil {
        fmt.Fprintf(os.Stderr, "[my-hook] Error reading stdin: %v\n", err)
        os.Exit(0)
    }

    var data HookInput
    if err := json.Unmarshal(input, &data); err != nil {
        fmt.Fprintf(os.Stderr, "[my-hook] Error parsing JSON: %v\n", err)
        os.Exit(0)
    }

    // Your hook logic here

    os.Exit(0) // Always exit 0
}
```

**Option B: Shell Script**

```bash
#!/usr/bin/env bash
set -euo pipefail

# Read stdin (optional)
input=$(cat)

# Your hook logic here

exit 0  # Always exit 0
```

### Step 3: Build (if Go)

```bash
cd /home/vincent/src/home/tools/claude-hooks
go build -o bin/my-custom-hook ./cmd/my-custom-hook
sudo cp bin/my-custom-hook /usr/local/bin/
```

Or add to `default.nix` for Nix packaging.

### Step 4: Add to settings.json

```json
{
  "hooks": {
    "SessionStart": [
      {
        "hooks": [
          {
            "type": "command",
            "command": "my-custom-hook"
          }
        ]
      }
    ]
  }
}
```

### Step 5: Test

```bash
# Test hook directly
echo '{"conversation_id":"test"}' | my-custom-hook

# Check output in terminal
```

### Step 6: Restart Claude Code
Hooks are loaded at startup. Restart to apply changes.

---

## Hook Development Best Practices

### 1. **Fast Execution**
- Hooks should complete in < 500ms
- For slow work, write to file and exit immediately
- Never wait for external services unless they respond quickly

### 2. **Graceful Failure**
- Always wrap in error handling
- Log errors to stderr (visible in terminal)
- Always `os.Exit(0)` - never exit(1) or panic

### 3. **Non-Blocking**
- Never wait for external services
- Fail silently if optional operations fail
- Don't disrupt Claude Code's core functionality

### 4. **Stdin Reading**
- Handle empty input gracefully
- Parse JSON with error handling
- Don't assume stdin will always have data

```go
input, err := io.ReadAll(os.Stdin)
if err != nil || len(input) == 0 {
    os.Exit(0)
}
```

### 5. **File I/O**
- Check file existence before reading
- Create directories with `os.MkdirAll(..., 0755)`
- Use append mode for log files

```go
dir := filepath.Join(os.Getenv("HOME"), ".config", ".claude", "history")
os.MkdirAll(dir, 0755)
f, _ := os.OpenFile(path, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
```

### 6. **Environment Access**
- Read environment variables with `os.Getenv()`
- Provide defaults for missing variables
- Use `CLAUDE_DIR` to override default `~/.config/claude` path

### 7. **Testing**
- Test hooks manually with sample input
- Use `go test` for unit tests
- Verify hooks work in actual Claude Code sessions

---

## Troubleshooting

### Hook Not Running

**Check:**
1. Is hook binary in PATH? `which claude-hooks-initialize-session`
2. Is path correct in settings.json? Use exact command name
3. Is settings.json valid JSON? `jq . ~/.config/claude/settings.json`
4. Did you restart Claude Code after editing settings.json?

**Debug:**
```bash
# Test hook directly
claude-hooks-initialize-session

# Test with input
echo '{"conversation_id":"test"}' | claude-hooks-capture-tool-output

# Check stderr output in terminal (hooks log there)
```

---

### Hook Hangs/Freezes Claude Code

**Cause:** Hook not exiting (infinite loop, waiting for input, blocking operation)

**Fix:**
1. Ensure `os.Exit(0)` is always reached
2. Add timeouts to all blocking operations
3. Check stdin reading doesn't hang

**Prevention:**
```go
// Add timeout
go func() {
    time.Sleep(5 * time.Second)
    fmt.Fprintln(os.Stderr, "[hook] Timeout - exiting")
    os.Exit(0)
}()

// Main logic here
```

---

### Permission Errors

**Check:**
```bash
# Ensure directories exist
mkdir -p ~/.config/claude/history/sessions
mkdir -p ~/.config/claude/history/tool-outputs

# Check write permissions
ls -la ~/.config/claude/history/

# Check binary permissions
ls -la $(which claude-hooks-initialize-session)
```

---

### No Logs Generated

**Check:**
1. Does `~/.config/claude/history/` directory exist?
2. Are hooks actually running? (Check terminal stderr output)
3. File permissions? `ls -la ~/.config/claude/history/`

**Debug:**
```bash
# Check recent logs
ls -lt ~/.config/claude/history/sessions/$(date +%Y-%m)/ | head -10
ls -lt ~/.config/claude/history/tool-outputs/$(date +%Y-%m)/ | head -10

# Check if tool-outputs JSONL has entries
tail ~/.config/claude/history/tool-outputs/$(date +%Y-%m)/$(date +%Y-%m-%d)_tool-outputs.jsonl
```

---

### Build Errors (Go)

**Check:**
```bash
# Check Go version
go version  # Requires Go 1.23+

# Update dependencies
cd /home/vincent/src/home/tools/claude-hooks
go mod tidy

# Rebuild
go build ./cmd/initialize-session
```

**Nix Build:**
```bash
cd /home/vincent/src/home
nix build .#claude-hooks

# If vendorHash error, update in default.nix
```

---

## Advantages of Go Implementation

1. **Zero runtime dependencies** - Compiled to native binary, no Bun/Node.js required
2. **Faster execution** - Native code vs interpreted JavaScript
3. **Easier distribution** - Single binary per hook
4. **Cross-compilation** - Build for any architecture from any platform
5. **Nix integration** - Proper package management and reproducible builds
6. **Type safety** - Compile-time checks without runtime overhead
7. **Standard library** - Excellent built-in support for JSON, file I/O, paths
8. **Smaller memory footprint** - Go binaries use less memory than Node.js processes

---

## Related Documentation

- **Hook Source Code:** `/home/vincent/src/home/tools/claude-hooks/`
- **Nix Package:** `/home/vincent/src/home/tools/claude-hooks/default.nix`
- **Configuration:** `/home/vincent/.config/claude/settings.json`
- **History Directory:** `~/.config/claude/history/`

---

## Quick Reference Card

```
HOOK LIFECYCLE:
1. Event occurs (SessionStart, PostToolUse, SessionEnd)
2. Claude Code executes hook command
3. Hook receives JSON data on stdin
4. Hook performs actions (log, set title, etc.)
5. Hook exits 0 (always succeeds)
6. Claude Code continues

KEY FILES:
/home/vincent/.config/claude/settings.json        Hook configuration
/home/vincent/src/home/tools/claude-hooks/        Hook source code
~/.config/claude/history/sessions/                       Session logs
~/.config/claude/history/tool-outputs/                   Tool output JSONL logs

CONFIGURED HOOKS:
claude-hooks-initialize-session    SessionStart - Set title, log session
claude-hooks-capture-tool-output   PostToolUse - Log tool executions
claude-hooks-save-session          SessionEnd - Prompt to save summary

VERIFY HOOKS:
which claude-hooks-initialize-session
jq '.hooks' ~/.config/claude/settings.json
ls -la ~/.config/claude/history/tool-outputs/

DEBUGGING:
# Test hooks manually
claude-hooks-initialize-session
echo '{"tool_name":"Bash","tool_input":{}}' | claude-hooks-capture-tool-output

# Check logs
tail ~/.config/claude/history/tool-outputs/$(date +%Y-%m)/$(date +%Y-%m-%d)_tool-outputs.jsonl
cat ~/.config/claude/history/sessions/$(date +%Y-%m)/$(date +%Y-%m-%d)_session-log.txt
```

---

**Last Updated:** 2024-12-04
**Implementation:** Go-based hooks (migrated from TypeScript/Bun)
**Status:** Active - 3 hooks configured (SessionStart, PostToolUse, SessionEnd)
