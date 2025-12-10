package main

import (
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"strings"
)

// TodoItem represents a single TODO item from TodoWrite
type TodoItem struct {
	Content    string `json:"content"`
	Status     string `json:"status"`
	ActiveForm string `json:"activeForm"`
}

// ToolResult represents the structure of tool execution results from PostToolUse hook
type ToolResult struct {
	ToolName  string          `json:"tool_name"`
	ToolInput json.RawMessage `json:"tool_input"`
}

// TodoWriteParams represents TodoWrite tool parameters
type TodoWriteParams struct {
	Todos []TodoItem `json:"todos"`
}

// SkillParams represents Skill tool parameters
type SkillParams struct {
	Skill string `json:"skill"`
}

// setTerminalTitle sets the terminal tab title using ANSI escape codes
func setTerminalTitle(title string) {
	fmt.Fprintf(os.Stderr, "\x1b]0;%s\x07", title)
	fmt.Fprintf(os.Stderr, "\x1b]2;%s\x07", title)
	fmt.Fprintf(os.Stderr, "\x1b]30;%s\x07", title)
}

// getProjectName extracts the project name from CLAUDE_PROJECT_DIR
func getProjectName() string {
	projectDir := os.Getenv("CLAUDE_PROJECT_DIR")
	if projectDir == "" {
		return ""
	}
	return filepath.Base(projectDir)
}

// getSkillIcon returns an emoji icon for known skills
func getSkillIcon(skillName string) string {
	icons := map[string]string{
		"Journal":    "📓",
		"Notes":      "📝",
		"TODOs":      "✅",
		"Org":        "📋",
		"Git":        "🔧",
		"GitHub":     "🐙",
		"Email":      "📧",
		"Python":     "🐍",
		"golang":     "🐹",
		"Rust":       "🦀",
		"Nix":        "❄️",
		"Kubernetes": "☸️",
		"Tekton":     "🚀",
	}

	if icon, ok := icons[skillName]; ok {
		return icon
	}
	return "🤖"
}

// getTodoProgress extracts task progress from TodoWrite parameters
func getTodoProgress(todos []TodoItem) (string, bool) {
	var inProgressTask string
	completedCount := 0
	total := len(todos)

	if total == 0 {
		return "", false
	}

	for _, todo := range todos {
		if todo.Status == "completed" {
			completedCount++
		} else if todo.Status == "in_progress" {
			inProgressTask = todo.ActiveForm
		}
	}

	if inProgressTask != "" {
		progress := fmt.Sprintf("[%d/%d] %s", completedCount+1, total, inProgressTask)
		return progress, true
	}

	return "", false
}

// buildTitle constructs the terminal title based on available context
func buildTitle(toolResult *ToolResult) string {
	var parts []string

	// Priority 1: Check for active task from TodoWrite
	if toolResult != nil && toolResult.ToolName == "TodoWrite" {
		var params TodoWriteParams
		if err := json.Unmarshal(toolResult.ToolInput, &params); err == nil {
			if progress, ok := getTodoProgress(params.Todos); ok {
				parts = append(parts, progress)
			}
		}
	}

	// Priority 2: Check for active skill
	if toolResult != nil && toolResult.ToolName == "Skill" {
		var params SkillParams
		if err := json.Unmarshal(toolResult.ToolInput, &params); err == nil {
			icon := getSkillIcon(params.Skill)
			parts = append(parts, fmt.Sprintf("%s %s", icon, params.Skill))
		}
	}

	// Priority 3: Add project context
	if projectName := getProjectName(); projectName != "" {
		parts = append(parts, projectName)
	}

	// Build final title
	if len(parts) > 0 {
		return fmt.Sprintf("Claude: %s", strings.Join(parts, " • "))
	}

	return "Claude Ready"
}

func main() {
	// Read tool result from stdin (if provided by hook system)
	var toolResult ToolResult
	decoder := json.NewDecoder(os.Stdin)

	// Try to decode, but don't fail if stdin is empty
	if err := decoder.Decode(&toolResult); err != nil {
		// No tool result provided, just use project context
		title := buildTitle(nil)
		setTerminalTitle(title)
		return
	}

	// Build and set title based on tool result
	title := buildTitle(&toolResult)
	setTerminalTitle(title)
}
