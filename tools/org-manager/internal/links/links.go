// Package links provides functionality for checking links in org-mode files.
package links

import (
	"bufio"
	"encoding/json"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/spf13/cobra"
)

// LinkError represents a broken or invalid link.
type LinkError struct {
	File    string `json:"file"`
	Line    int    `json:"line"`
	Link    string `json:"link"`
	Type    string `json:"type"`
	Message string `json:"message"`
}

func (l LinkError) String() string {
	return fmt.Sprintf("%s:%d: [%s] %s - %s", l.File, l.Line, l.Type, l.Link, l.Message)
}

// Options holds configuration for link checking operations.
type Options struct {
	Format  string
	Verbose bool
}

// NewCommand creates the check-links subcommand.
func NewCommand(orgDir *string) *cobra.Command {
	opts := &Options{}

	cmd := &cobra.Command{
		Use:   "check-links",
		Short: "Check for invalid links (local, denote, etc.)",
		Long: `Check org files for broken or invalid links including:
- Local file links (file:...)
- Denote links (denote:IDENTIFIER)
- ID links (id:...)
- HTTP/HTTPS links (basic validation)

The checker will verify that linked files exist and that
denote identifiers reference existing notes.`,
		Example: `  # Check links with default text output
  org-manager check-links

  # Output results as JSON
  org-manager check-links --format json

  # Verbose output showing all checked links
  org-manager check-links --verbose`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return Run(opts, *orgDir)
		},
	}

	cmd.Flags().StringVarP(&opts.Format, "format", "f", "text", "Output format: text or json")
	cmd.Flags().BoolVarP(&opts.Verbose, "verbose", "v", false, "Show verbose output including valid links")

	return cmd
}

// Run executes the link checking operation.
func Run(opts *Options, orgDir string) error {
	notesDir := filepath.Join(orgDir, "notes")
	if _, err := os.Stat(notesDir); os.IsNotExist(err) {
		return fmt.Errorf("notes directory does not exist: %s", notesDir)
	}

	var errors []LinkError
	filesChecked := 0

	// Build index of identifiers for denote link checking
	identifierIndex := make(map[string]string)
	err := filepath.Walk(notesDir, func(path string, info os.FileInfo, err error) error {
		if err != nil || info.IsDir() || !strings.HasSuffix(path, ".org") {
			return err
		}
		if id := extractIdentifier(path); id != "" {
			identifierIndex[id] = path
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to build identifier index: %w", err)
	}

	// Check links in each file
	err = filepath.Walk(notesDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() || !strings.HasSuffix(path, ".org") {
			return nil
		}

		if opts.Verbose {
			fmt.Printf("Checking links in %s...\n", path)
		}

		linkErrors := checkLinksInFile(path, notesDir, identifierIndex, opts.Verbose)
		errors = append(errors, linkErrors...)
		filesChecked++
		return nil
	})

	if err != nil {
		return fmt.Errorf("failed to scan directory: %w", err)
	}

	// Output results
	if opts.Format == "json" {
		return outputJSON(errors, filesChecked)
	}
	return outputText(errors, filesChecked)
}

func checkLinksInFile(path, notesDir string, identifierIndex map[string]string, verbose bool) []LinkError {
	var errors []LinkError

	file, err := os.Open(path)
	if err != nil {
		errors = append(errors, LinkError{
			File:    path,
			Line:    0,
			Type:    "error",
			Message: fmt.Sprintf("failed to open file: %v", err),
		})
		return errors
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineNum := 0
	linkPattern := regexp.MustCompile(`\[\[([^\]]+)\](?:\[([^\]]+)\])?\]`)

	for scanner.Scan() {
		lineNum++
		line := scanner.Text()

		matches := linkPattern.FindAllStringSubmatch(line, -1)
		for _, match := range matches {
			link := match[1]
			linkType, target := parseLinkType(link)

			if verbose {
				fmt.Printf("  Line %d: %s link: %s\n", lineNum, linkType, target)
			}

			switch linkType {
			case "file":
				var absPath string
				if filepath.IsAbs(target) {
					absPath = target
				} else {
					absPath = filepath.Join(filepath.Dir(path), target)
				}
				if _, err := os.Stat(absPath); os.IsNotExist(err) {
					errors = append(errors, LinkError{
						File:    path,
						Line:    lineNum,
						Link:    link,
						Type:    "file",
						Message: "file does not exist",
					})
				}

			case "denote":
				if _, exists := identifierIndex[target]; !exists {
					errors = append(errors, LinkError{
						File:    path,
						Line:    lineNum,
						Link:    link,
						Type:    "denote",
						Message: "identifier not found",
					})
				}

			case "id":
				if len(target) < 10 {
					errors = append(errors, LinkError{
						File:    path,
						Line:    lineNum,
						Link:    link,
						Type:    "id",
						Message: "invalid ID format (too short)",
					})
				}

			case "http", "https":
				if target == "" {
					errors = append(errors, LinkError{
						File:    path,
						Line:    lineNum,
						Link:    link,
						Type:    linkType,
						Message: "empty URL",
					})
				}
			}
		}
	}

	if err := scanner.Err(); err != nil {
		errors = append(errors, LinkError{
			File:    path,
			Message: fmt.Sprintf("error reading file: %v", err),
		})
	}

	return errors
}

func parseLinkType(link string) (string, string) {
	if strings.HasPrefix(link, "file:") {
		return "file", strings.TrimPrefix(link, "file:")
	}
	if strings.HasPrefix(link, "denote:") {
		return "denote", strings.TrimPrefix(link, "denote:")
	}
	if strings.HasPrefix(link, "id:") {
		return "id", strings.TrimPrefix(link, "id:")
	}
	if strings.HasPrefix(link, "http://") {
		return "http", link
	}
	if strings.HasPrefix(link, "https://") {
		return "https", link
	}
	return "file", link
}

func extractIdentifier(path string) string {
	file, err := os.Open(path)
	if err != nil {
		return ""
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	identifierPattern := regexp.MustCompile(`^#\+identifier:\s*(.+)$`)

	for scanner.Scan() {
		line := strings.ToLower(scanner.Text())
		if matches := identifierPattern.FindStringSubmatch(line); matches != nil {
			return strings.TrimSpace(matches[1])
		}
		if !strings.HasPrefix(line, "#+") && strings.TrimSpace(line) != "" {
			break
		}
	}

	return ""
}

func outputJSON(errors []LinkError, filesChecked int) error {
	result := map[string]interface{}{
		"files_checked": filesChecked,
		"errors_found":  len(errors),
		"errors":        errors,
	}

	encoder := json.NewEncoder(os.Stdout)
	encoder.SetIndent("", "  ")
	return encoder.Encode(result)
}

func outputText(errors []LinkError, filesChecked int) error {
	fmt.Printf("\nLink check complete: checked %d files\n", filesChecked)

	if len(errors) == 0 {
		fmt.Println("No broken links found!")
		return nil
	}

	fmt.Printf("\nFound %d broken links:\n", len(errors))
	for _, e := range errors {
		fmt.Println(e.String())
	}

	return fmt.Errorf("link check failed with %d errors", len(errors))
}
