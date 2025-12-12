package main

import (
	"bufio"
	"fmt"
	"os"
	"os/exec"
	"regexp"
	"strings"
)

func main() {
	if len(os.Args) < 2 {
		fmt.Fprintln(os.Stderr, "Usage: cliphist-cleanup <pattern> [pattern2] [pattern3] ...")
		fmt.Fprintln(os.Stderr, "")
		fmt.Fprintln(os.Stderr, "Examples:")
		fmt.Fprintln(os.Stderr, "  cliphist-cleanup 'Signed-off-by:'")
		fmt.Fprintln(os.Stderr, "  cliphist-cleanup '^# This is' 'Co-Authored-By:'")
		fmt.Fprintln(os.Stderr, "")
		fmt.Fprintln(os.Stderr, "Patterns are treated as regular expressions (case-insensitive).")
		os.Exit(1)
	}

	// Compile patterns
	patterns := make([]*regexp.Regexp, 0, len(os.Args)-1)
	for _, pattern := range os.Args[1:] {
		re, err := regexp.Compile("(?i)" + pattern)
		if err != nil {
			fmt.Fprintf(os.Stderr, "Error compiling pattern '%s': %v\n", pattern, err)
			os.Exit(1)
		}
		patterns = append(patterns, re)
	}

	// Get clipboard history
	cmd := exec.Command("cliphist", "list")
	stdout, err := cmd.StdoutPipe()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error creating pipe: %v\n", err)
		os.Exit(1)
	}

	if err := cmd.Start(); err != nil {
		fmt.Fprintf(os.Stderr, "Error starting cliphist list: %v\n", err)
		os.Exit(1)
	}

	// Read entries and delete matching ones
	scanner := bufio.NewScanner(stdout)
	deleted := 0
	checked := 0

	for scanner.Scan() {
		line := scanner.Text()
		checked++

		// Check if line matches any pattern
		matched := false
		for _, re := range patterns {
			if re.MatchString(line) {
				matched = true
				break
			}
		}

		if matched {
			// Delete this entry using delete-query
			// The line format from cliphist list is: "ID\tCONTENT"
			// We need to extract the content part after the tab
			parts := strings.SplitN(line, "\t", 2)
			if len(parts) < 2 {
				continue
			}

			content := parts[1]
			deleteCmd := exec.Command("cliphist", "delete-query", content)
			if err := deleteCmd.Run(); err != nil {
				fmt.Fprintf(os.Stderr, "Warning: Failed to delete entry: %v\n", err)
			} else {
				deleted++
				fmt.Printf("Deleted: %s\n", truncate(content, 80))
			}
		}
	}

	if err := scanner.Err(); err != nil {
		fmt.Fprintf(os.Stderr, "Error reading cliphist output: %v\n", err)
		os.Exit(1)
	}

	if err := cmd.Wait(); err != nil {
		fmt.Fprintf(os.Stderr, "Error waiting for cliphist: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("\nChecked %d entries, deleted %d entries\n", checked, deleted)
}

func truncate(s string, maxLen int) string {
	// Replace newlines with spaces for display
	s = strings.ReplaceAll(s, "\n", " ")
	s = strings.ReplaceAll(s, "\r", " ")

	if len(s) <= maxLen {
		return s
	}
	return s[:maxLen-3] + "..."
}
