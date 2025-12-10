package main

import (
	"fmt"
	"os"
	"os/exec"
)

// isSubagentSession checks if this is a subagent session
func isSubagentSession() bool {
	claudeProjectDir := os.Getenv("CLAUDE_PROJECT_DIR")
	if len(claudeProjectDir) > 0 && (len(claudeProjectDir) < 2 || claudeProjectDir[len(claudeProjectDir)-2:] != "/.") {
		return true
	}
	if os.Getenv("CLAUDE_AGENT_TYPE") != "" {
		return true
	}
	return false
}

func main() {
	// Check if this is a subagent session
	if isSubagentSession() {
		// Silent exit for subagent sessions
		os.Exit(0)
	}

	// Send desktop notification
	cmd := exec.Command("notify-send", "-u", "low", "Claude Code", "Session ending")
	if err := cmd.Run(); err != nil {
		// Silent failure - don't break workflow
		fmt.Fprintf(os.Stderr, "[save-session] Warning: Could not send notification: %v\n", err)
	}

	// Get session statistics
	statsCmd := exec.Command("claude-hooks-session-stats")
	statsCmd.Env = os.Environ()
	statsOutput, err := statsCmd.Output()
	if err != nil {
		// Continue without stats if tool fails
		fmt.Fprintf(os.Stderr, "[save-session] Warning: Could not get session stats: %v\n", err)
	}

	// Output directive for Claude to save the session
	// This will be shown in the conversation
	fmt.Println("")
	fmt.Println("---")
	fmt.Println("")
	fmt.Println("# Automatic Session Summary")
	fmt.Println("")
	fmt.Println("**IMPORTANT**: Please create a session summary and save it to the history directory.")
	fmt.Println("")

	// Show statistics if available
	if len(statsOutput) > 0 {
		fmt.Print(string(statsOutput))
	}

	fmt.Println("## Instructions")
	fmt.Println("")
	fmt.Println("Create a brief summary (2-4 paragraphs) of this session documenting:")
	fmt.Println("- Primary tasks accomplished")
	fmt.Println("- Key decisions or solutions")
	fmt.Println("- Files/systems modified")
	fmt.Println("- Any remaining work or next steps")
	fmt.Println("")
	fmt.Println("Save the summary using the Write tool to:")
	fmt.Println("`~/.config/claude/history/sessions/<YYYY-MM>/<YYYY-MM-DD>_<brief-slug>.md`")
	fmt.Println("")
	fmt.Println("Use the format: `YYYY-MM-DD_<2-4 word slug describing the session>.md`")
	fmt.Println("")
	fmt.Println("Example: `~/.config/claude/history/sessions/2025-12/2025-12-10_notification-filtering-arr-completion.md`")
	fmt.Println("")

	os.Exit(0)
}
