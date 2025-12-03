package main

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"time"

	"github.com/vdemeester/home/tools/claude-hooks/internal/paths"
)

// ToolUseData represents the input from PostToolUse hook
type ToolUseData struct {
	ToolName       string                 `json:"tool_name"`
	ToolInput      map[string]interface{} `json:"tool_input"`
	ToolResponse   map[string]interface{} `json:"tool_response"`
	ConversationID string                 `json:"conversation_id"`
	Timestamp      string                 `json:"timestamp"`
}

// CaptureEntry represents a log entry in JSONL format
type CaptureEntry struct {
	Timestamp string                 `json:"timestamp"`
	Tool      string                 `json:"tool"`
	Input     map[string]interface{} `json:"input"`
	Output    map[string]interface{} `json:"output"`
	Session   string                 `json:"session"`
}

// List of tools to capture
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

func main() {
	// Read input from stdin
	input, err := io.ReadAll(os.Stdin)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[capture-tool-output] Error reading stdin: %v\n", err)
		os.Exit(0) // Silent failure - don't disrupt workflow
	}

	if len(input) == 0 {
		os.Exit(0)
	}

	var data ToolUseData
	if err := json.Unmarshal(input, &data); err != nil {
		fmt.Fprintf(os.Stderr, "[capture-tool-output] Error parsing JSON: %v\n", err)
		os.Exit(0) // Silent failure
	}

	// Only capture interesting tools
	if !interestingTools[data.ToolName] {
		os.Exit(0)
	}

	// Get today's date for organization
	now := time.Now()
	today := now.Format("2006-01-02")
	yearMonth := now.Format("2006-01")

	// Ensure capture directory exists
	dateDir := filepath.Join(paths.HistoryDir(), "tool-outputs", yearMonth)
	if err := os.MkdirAll(dateDir, 0755); err != nil {
		fmt.Fprintf(os.Stderr, "[capture-tool-output] Error creating directory: %v\n", err)
		os.Exit(0) // Silent failure
	}

	// Format output as JSONL
	captureFile := filepath.Join(dateDir, fmt.Sprintf("%s_tool-outputs.jsonl", today))

	timestamp := data.Timestamp
	if timestamp == "" {
		timestamp = now.Format(time.RFC3339)
	}

	entry := CaptureEntry{
		Timestamp: timestamp,
		Tool:      data.ToolName,
		Input:     data.ToolInput,
		Output:    data.ToolResponse,
		Session:   data.ConversationID,
	}

	jsonData, err := json.Marshal(entry)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[capture-tool-output] Error marshaling JSON: %v\n", err)
		os.Exit(0) // Silent failure
	}

	// Append to daily log
	f, err := os.OpenFile(captureFile, os.O_APPEND|os.O_CREATE|os.O_WRONLY, 0644)
	if err != nil {
		fmt.Fprintf(os.Stderr, "[capture-tool-output] Error opening file: %v\n", err)
		os.Exit(0) // Silent failure
	}
	defer f.Close()

	if _, err := f.WriteString(string(jsonData) + "\n"); err != nil {
		fmt.Fprintf(os.Stderr, "[capture-tool-output] Error writing to file: %v\n", err)
		os.Exit(0) // Silent failure
	}

	os.Exit(0)
}
