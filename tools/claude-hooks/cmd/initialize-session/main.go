package main

import (
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"time"

	"github.com/vdemeester/home/tools/claude-hooks/internal/paths"
)

const (
	debounceDuration = 2 * time.Second
)

func getLockfile() string {
	return filepath.Join(os.TempDir(), "claude-session-start.lock")
}

// shouldDebounce checks if we're within the debounce window
func shouldDebounce() bool {
	lockfile := getLockfile()

	data, err := os.ReadFile(lockfile)
	if err == nil {
		lockTime, err := strconv.ParseInt(strings.TrimSpace(string(data)), 10, 64)
		if err == nil {
			now := time.Now().UnixMilli()
			if now-lockTime < debounceDuration.Milliseconds() {
				return true
			}
		}
	}

	// Update lockfile with current timestamp
	now := time.Now().UnixMilli()
	if err := os.WriteFile(lockfile, []byte(fmt.Sprintf("%d", now)), 0644); err != nil {
		// Ignore write errors
	}

	return false
}

// isSubagentSession checks if this is a subagent session
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

// setTerminalTitle sets the terminal tab title using ANSI escape codes
func setTerminalTitle(title string) {
	fmt.Fprintf(os.Stderr, "\x1b]0;%s\x07", title)
	fmt.Fprintf(os.Stderr, "\x1b]2;%s\x07", title)
	fmt.Fprintf(os.Stderr, "\x1b]30;%s\x07", title)
}

// logSessionStart logs the session start to history
func logSessionStart() error {
	timestamp := paths.GetTimestamp()
	yearMonth := timestamp[:7] // YYYY-MM

	logDir := filepath.Join(paths.HistoryDir(), "sessions", yearMonth)
	if err := os.MkdirAll(logDir, 0755); err != nil {
		return err
	}

	logEntry := fmt.Sprintf("%s - Session started\n", time.Now().Format(time.RFC3339))
	logFile := filepath.Join(logDir, fmt.Sprintf("%s_session-log.txt", timestamp[:10]))

	f, err := os.OpenFile(logFile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		return err
	}
	defer f.Close()

	_, err = f.WriteString(logEntry)
	return err
}

// loadCoreSkill outputs the CORE skill content so Claude receives it at session start
func loadCoreSkill() error {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return err
	}

	coreSkillPath := filepath.Join(homeDir, ".config/claude/skills/CORE/SKILL.md")
	content, err := os.ReadFile(coreSkillPath)
	if err != nil {
		return err
	}

	// Output to stdout so Claude receives it
	fmt.Println("\n<!-- CORE Skill Auto-Loaded at Session Start -->")
	fmt.Println(string(content))
	fmt.Println("<!-- End CORE Skill -->")

	return nil
}

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

	// Load CORE skill at session start
	if err := loadCoreSkill(); err != nil {
		// Warn but don't break session start
		fmt.Fprintf(os.Stderr, "[initialize-session] Warning: Could not load CORE skill: %v\n", err)
	}

	// Send desktop notification
	cmd := exec.Command("notify-send", "-u", "low", "Claude Code", "Session started")
	if err := cmd.Run(); err != nil {
		// Silent failure - don't break workflow
		fmt.Fprintf(os.Stderr, "[initialize-session] Warning: Could not send notification: %v\n", err)
	}

	// Log session start to history (silent failure)
	if err := logSessionStart(); err != nil {
		// Don't break session start for logging issues
		fmt.Fprintf(os.Stderr, "[initialize-session] Warning: Could not log session start: %v\n", err)
	}

	os.Exit(0)
}
