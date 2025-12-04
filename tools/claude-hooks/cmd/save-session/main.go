package main

import (
	"fmt"
	"os"
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

	// Output prompt for Claude to save the session
	// This will be shown in the conversation
	fmt.Println("")
	fmt.Println("---")
	fmt.Println("")
	fmt.Println("**Session ending**. Would you like me to save a summary of this session to your history?")
	fmt.Println("")
	fmt.Println("I can create a session entry in `~/.config/claude/history/sessions/` documenting:")
	fmt.Println("- What was accomplished")
	fmt.Println("- Decisions made")
	fmt.Println("- Next steps")
	fmt.Println("- Related notes")
	fmt.Println("")

	os.Exit(0)
}
