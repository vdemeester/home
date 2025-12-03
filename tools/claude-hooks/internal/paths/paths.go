package paths

import (
	"fmt"
	"os"
	"path/filepath"
	"time"
)

// ClaudeDir returns the base Claude directory (~/.claude or CLAUDE_DIR env var)
func ClaudeDir() string {
	if dir := os.Getenv("CLAUDE_DIR"); dir != "" {
		return dir
	}
	home, err := os.UserHomeDir()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting home directory: %v\n", err)
		os.Exit(1)
	}
	return filepath.Join(home, ".claude")
}

// HooksDir returns the hooks directory
func HooksDir() string {
	return filepath.Join(ClaudeDir(), "hooks")
}

// SkillsDir returns the skills directory
func SkillsDir() string {
	return filepath.Join(ClaudeDir(), "skills")
}

// AgentsDir returns the agents directory
func AgentsDir() string {
	return filepath.Join(ClaudeDir(), "agents")
}

// HistoryDir returns the history directory
func HistoryDir() string {
	return filepath.Join(ClaudeDir(), "history")
}

// GetHistoryFilePath returns a history file path with year-month organization
func GetHistoryFilePath(subdir, filename string) string {
	now := time.Now()
	yearMonth := now.Format("2006-01")
	return filepath.Join(HistoryDir(), subdir, yearMonth, filename)
}

// GetTimestamp returns current timestamp in YYYY-MM-DD-HHMMSS format
func GetTimestamp() string {
	return time.Now().Format("2006-01-02-150405")
}

// GetDate returns current date in YYYY-MM-DD format
func GetDate() string {
	return time.Now().Format("2006-01-02")
}

// GetYearMonth returns current year-month in YYYY-MM format
func GetYearMonth() string {
	return time.Now().Format("2006-01")
}

// ValidateClaudeStructure validates that the Claude directory exists
func ValidateClaudeStructure() error {
	claudeDir := ClaudeDir()
	if _, err := os.Stat(claudeDir); os.IsNotExist(err) {
		return fmt.Errorf("CLAUDE_DIR does not exist: %s\nExpected ~/.claude or set CLAUDE_DIR environment variable", claudeDir)
	}
	return nil
}

// EnsureHistoryDir creates the history directory structure if needed
func EnsureHistoryDir(subdir string) error {
	yearMonth := GetYearMonth()
	dir := filepath.Join(HistoryDir(), subdir, yearMonth)
	return os.MkdirAll(dir, 0755)
}
