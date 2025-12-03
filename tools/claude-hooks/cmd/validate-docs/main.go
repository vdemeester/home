package main

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/bmatcuk/doublestar/v4"
)

// ANSI color codes
const (
	colorReset  = "\x1b[0m"
	colorRed    = "\x1b[31m"
	colorGreen  = "\x1b[32m"
	colorYellow = "\x1b[33m"
	colorCyan   = "\x1b[36m"
)

type brokenLink struct {
	file   string
	link   string
	target string
	line   int
}

// extractLinks extracts markdown links from content
func extractLinks(content string) []struct {
	link string
	line int
} {
	var links []struct {
		link string
		line int
	}

	// Match [text](path) style links
	linkRegex := regexp.MustCompile(`\[([^\]]+)\]\(([^)]+)\)`)

	scanner := bufio.NewScanner(strings.NewReader(content))
	lineNum := 0
	for scanner.Scan() {
		lineNum++
		line := scanner.Text()

		matches := linkRegex.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			if len(match) >= 3 {
				link := match[2]

				// Skip external URLs, anchors, and mailto links
				if strings.HasPrefix(link, "http://") ||
					strings.HasPrefix(link, "https://") ||
					strings.HasPrefix(link, "#") ||
					strings.HasPrefix(link, "mailto:") {
					continue
				}

				links = append(links, struct {
					link string
					line int
				}{link: link, line: lineNum})
			}
		}
	}

	return links
}

// resolveLink resolves a link path relative to the file
func resolveLink(fromFile, linkPath, baseDir string) string {
	// Remove anchor if present
	parts := strings.Split(linkPath, "#")
	pathWithoutAnchor := parts[0]

	// If it starts with ~, expand to home directory
	if strings.HasPrefix(pathWithoutAnchor, "~/") {
		home, err := os.UserHomeDir()
		if err != nil {
			return pathWithoutAnchor
		}
		return filepath.Join(home, pathWithoutAnchor[2:])
	}

	// If it's absolute, use as-is
	if filepath.IsAbs(pathWithoutAnchor) {
		return pathWithoutAnchor
	}

	// Otherwise, resolve relative to the file's directory
	fileDir := filepath.Dir(fromFile)
	return filepath.Join(fileDir, pathWithoutAnchor)
}

// validateDocs validates markdown files in a directory
func validateDocs(baseDir string) []brokenLink {
	var broken []brokenLink

	// Find all markdown files using doublestar glob
	pattern := filepath.Join(baseDir, "**/*.md")
	matches, err := doublestar.FilepathGlob(pattern)
	if err != nil {
		fmt.Fprintf(os.Stderr, "%sError globbing files: %v%s\n", colorYellow, err, colorReset)
		return broken
	}

	for _, filePath := range matches {
		// Skip node_modules and hidden directories
		if strings.Contains(filePath, "node_modules") || strings.Contains(filePath, "/.") {
			continue
		}

		content, err := os.ReadFile(filePath)
		if err != nil {
			fmt.Fprintf(os.Stderr, "%sWarning: Could not read %s%s\n", colorYellow, filePath, colorReset)
			continue
		}

		links := extractLinks(string(content))
		for _, linkInfo := range links {
			targetPath := resolveLink(filePath, linkInfo.link, baseDir)

			// Check if target exists
			if _, err := os.Stat(targetPath); os.IsNotExist(err) {
				// Get relative path for display
				relPath, _ := filepath.Rel(baseDir, filePath)
				broken = append(broken, brokenLink{
					file:   relPath,
					link:   linkInfo.link,
					target: targetPath,
					line:   linkInfo.line,
				})
			}
		}
	}

	return broken
}

func main() {
	baseDir, err := os.Getwd()
	if err != nil {
		fmt.Fprintf(os.Stderr, "Error getting current directory: %v\n", err)
		os.Exit(1)
	}

	fmt.Printf("\n%s🔍 Documentation Link Validator%s\n", colorCyan, colorReset)
	fmt.Printf("%s   Base directory: %s%s\n\n", colorCyan, baseDir, colorReset)

	brokenLinks := validateDocs(baseDir)

	if len(brokenLinks) > 0 {
		fmt.Printf("\n%s❌ Found %d broken link(s):%s\n\n", colorRed, len(brokenLinks), colorReset)

		for _, broken := range brokenLinks {
			fmt.Printf("  %s%s:%d%s\n", colorYellow, broken.file, broken.line, colorReset)
			fmt.Printf("    → %s%s%s (not found)\n\n", colorRed, broken.link, colorReset)
		}

		fmt.Printf("\n%sDocumentation validation failed. Please fix the broken links.%s\n\n", colorRed, colorReset)
		os.Exit(1)
	}

	fmt.Printf("%s✅ All documentation links are valid%s\n\n", colorGreen, colorReset)
	os.Exit(0)
}
