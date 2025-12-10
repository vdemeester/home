package main

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"

	"github.com/vdemeester/home/tools/claude-hooks/internal/paths"
)

// CaptureEntry represents a log entry from tool-outputs JSONL
type CaptureEntry struct {
	Timestamp string                 `json:"timestamp"`
	Tool      string                 `json:"tool"`
	Input     map[string]interface{} `json:"input"`
	Output    map[string]interface{} `json:"output"`
	Session   string                 `json:"session"`
}

// SessionStats holds aggregated statistics
type SessionStats struct {
	ToolCounts    map[string]int
	FilesModified []string
	FilesRead     []string
	CommandsRun   []string
	StartTime     time.Time
	EndTime       time.Time
	Duration      time.Duration
}

func main() {
	conversationID := os.Getenv("CLAUDE_CONVERSATION_ID")

	// Get today's tool output file
	now := time.Now()
	today := now.Format("2006-01-02")
	yearMonth := now.Format("2006-01")

	toolOutputFile := filepath.Join(
		paths.HistoryDir(),
		"tool-outputs",
		yearMonth,
		fmt.Sprintf("%s_tool-outputs.jsonl", today),
	)

	stats := &SessionStats{
		ToolCounts:    make(map[string]int),
		FilesModified: []string{},
		FilesRead:     []string{},
		CommandsRun:   []string{},
	}

	// Read and parse JSONL
	f, err := os.Open(toolOutputFile)
	if err != nil {
		// Silent failure - file might not exist for new sessions
		fmt.Fprintln(os.Stderr, "[session-stats] No tool output file found")
		os.Exit(0)
	}
	defer f.Close()

	scanner := bufio.NewScanner(f)
	// Increase buffer size for large lines
	buf := make([]byte, 0, 64*1024)
	scanner.Buffer(buf, 1024*1024)

	firstEntry := true
	for scanner.Scan() {
		var entry CaptureEntry
		if err := json.Unmarshal(scanner.Bytes(), &entry); err != nil {
			continue // Skip malformed lines
		}

		// Only count entries from this session if we have a conversation ID
		if conversationID != "" && entry.Session != "" && entry.Session != conversationID {
			continue
		}

		stats.ToolCounts[entry.Tool]++

		// Track timestamps
		if t, err := time.Parse(time.RFC3339, entry.Timestamp); err == nil {
			if firstEntry {
				stats.StartTime = t
				firstEntry = false
			}
			stats.EndTime = t
		}

		// Extract file paths and commands based on tool type
		switch entry.Tool {
		case "Edit", "Write":
			if path, ok := entry.Input["file_path"].(string); ok {
				if !contains(stats.FilesModified, path) {
					stats.FilesModified = append(stats.FilesModified, path)
				}
			}
		case "Read":
			if path, ok := entry.Input["file_path"].(string); ok {
				if !contains(stats.FilesRead, path) {
					stats.FilesRead = append(stats.FilesRead, path)
				}
			}
		case "Bash":
			if cmd, ok := entry.Input["command"].(string); ok {
				// Only include interesting commands (not trivial ones)
				if isInterestingCommand(cmd) {
					// Truncate long commands
					if len(cmd) > 80 {
						cmd = cmd[:77] + "..."
					}
					stats.CommandsRun = append(stats.CommandsRun, cmd)
				}
			}
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "[session-stats] Error reading file: %v\n", err)
	}

	// Calculate duration
	if !stats.StartTime.IsZero() && !stats.EndTime.IsZero() {
		stats.Duration = stats.EndTime.Sub(stats.StartTime)
	}

	// Output statistics
	printStats(stats)
}

func contains(slice []string, item string) bool {
	for _, s := range slice {
		if s == item {
			return true
		}
	}
	return false
}

func isInterestingCommand(cmd string) bool {
	// Skip trivial commands
	boring := []string{
		"ls ", "pwd", "echo ", "cat ", "head ", "tail ",
		"git status", "git diff", "git log",
	}

	cmdLower := strings.ToLower(cmd)
	for _, b := range boring {
		if strings.HasPrefix(cmdLower, b) {
			return false
		}
	}

	return true
}

func printStats(stats *SessionStats) {
	fmt.Println("## Session Statistics")
	fmt.Println()

	// Duration
	if stats.Duration > 0 {
		fmt.Printf("**Duration:** %s\n", stats.Duration.Round(time.Second))
		fmt.Println()
	}

	// Tool usage
	if len(stats.ToolCounts) > 0 {
		fmt.Println("**Tools used:**")

		// Sort tools by count
		type toolCount struct {
			tool  string
			count int
		}
		var tools []toolCount
		for tool, count := range stats.ToolCounts {
			tools = append(tools, toolCount{tool, count})
		}
		sort.Slice(tools, func(i, j int) bool {
			return tools[i].count > tools[j].count
		})

		for _, tc := range tools {
			fmt.Printf("- %s: %d\n", tc.tool, tc.count)
		}
		fmt.Println()
	}

	// Files modified
	if len(stats.FilesModified) > 0 {
		fmt.Println("**Files modified:**")
		for _, f := range stats.FilesModified {
			// Shorten home directory paths
			f = strings.Replace(f, os.Getenv("HOME"), "~", 1)
			fmt.Printf("- %s\n", f)
		}
		fmt.Println()
	}

	// Notable commands (limit to 10)
	if len(stats.CommandsRun) > 0 {
		fmt.Println("**Commands executed:**")
		limit := len(stats.CommandsRun)
		if limit > 10 {
			limit = 10
		}
		for i := 0; i < limit; i++ {
			fmt.Printf("- `%s`\n", stats.CommandsRun[i])
		}
		if len(stats.CommandsRun) > 10 {
			fmt.Printf("- ... and %d more\n", len(stats.CommandsRun)-10)
		}
		fmt.Println()
	}

	// File reads (only if not too many)
	if len(stats.FilesRead) > 0 && len(stats.FilesRead) <= 15 {
		fmt.Println("**Files read:**")
		for _, f := range stats.FilesRead {
			f = strings.Replace(f, os.Getenv("HOME"), "~", 1)
			fmt.Printf("- %s\n", f)
		}
		fmt.Println()
	}
}
