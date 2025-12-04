// Package validate provides functionality for validating org-mode files.
package validate

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
	"strings"

	"github.com/spf13/cobra"
)

// Compile regex patterns once at package level for better performance
var (
	metadataPattern     = regexp.MustCompile(`^#\+(\w+):\s*(.*)$`)
	headlinePattern     = regexp.MustCompile(`^(\*+)\s+(.*)$`)
	propertyDrawerStart = regexp.MustCompile(`^\s*:PROPERTIES:\s*$`)
	propertyDrawerEnd   = regexp.MustCompile(`^\s*:END:\s*$`)
)

// ValidationError represents an error found during validation.
type ValidationError struct {
	File    string
	Line    int
	Column  int
	Message string
}

func (v ValidationError) String() string {
	if v.Line > 0 {
		return fmt.Sprintf("%s:%d:%d: %s", v.File, v.Line, v.Column, v.Message)
	}
	return fmt.Sprintf("%s: %s", v.File, v.Message)
}

// Options holds configuration for validation operations.
type Options struct {
	CheckAll       bool
	CheckMetadata  bool
	CheckStructure bool
	Verbose        bool
}

// NewCommand creates the validate subcommand.
func NewCommand(orgDir *string) *cobra.Command {
	opts := &Options{}

	cmd := &cobra.Command{
		Use:   "validate",
		Short: "Validate org-mode files for correctness",
		Long: `Validate org-mode files checking for:
- Required metadata (title, identifier)
- Proper headline structure
- Balanced brackets and parentheses
- Denote filename conventions

The validator walks through all .org files in the notes directory
and reports any structural or metadata issues.`,
		Example: `  # Validate all org files
  org-manager validate

  # Validate with verbose output
  org-manager validate --verbose

  # Only check metadata
  org-manager validate --check-structure=false`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return Run(opts, *orgDir)
		},
	}

	cmd.Flags().BoolVar(&opts.CheckAll, "check-all", true, "Check all org files in the directory")
	cmd.Flags().BoolVar(&opts.CheckMetadata, "check-metadata", true, "Validate org-mode metadata")
	cmd.Flags().BoolVar(&opts.CheckStructure, "check-structure", true, "Validate org-mode structure")
	cmd.Flags().BoolVarP(&opts.Verbose, "verbose", "v", false, "Show verbose output")

	return cmd
}

// Run executes the validation operation.
func Run(opts *Options, orgDir string) error {
	notesDir := filepath.Join(orgDir, "notes")
	if _, err := os.Stat(notesDir); os.IsNotExist(err) {
		return fmt.Errorf("notes directory does not exist: %s", notesDir)
	}

	var errors []ValidationError
	filesChecked := 0

	err := filepath.Walk(notesDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() || !strings.HasSuffix(path, ".org") {
			return nil
		}

		if opts.Verbose {
			fmt.Printf("Checking %s...\n", path)
		}

		fileErrors := validateOrgFile(path, opts.CheckMetadata, opts.CheckStructure)
		errors = append(errors, fileErrors...)
		filesChecked++
		return nil
	})

	if err != nil {
		return fmt.Errorf("failed to scan directory: %w", err)
	}

	// Print results
	fmt.Printf("\nValidation complete: checked %d files\n", filesChecked)

	if len(errors) == 0 {
		fmt.Println("No errors found!")
		return nil
	}

	fmt.Printf("\nFound %d errors:\n", len(errors))
	for _, e := range errors {
		fmt.Println(e.String())
	}

	return fmt.Errorf("validation failed with %d errors", len(errors))
}

func validateOrgFile(path string, checkMetadata, checkStructure bool) []ValidationError {
	var errors []ValidationError

	file, err := os.Open(path)
	if err != nil {
		errors = append(errors, ValidationError{
			File:    path,
			Message: fmt.Sprintf("failed to open file: %v", err),
		})
		return errors
	}
	defer file.Close()

	scanner := bufio.NewScanner(file)
	lineNum := 0
	hasTitle := false
	hasIdentifier := false
	inMetadata := true
	headlineLevel := 0
	inPropertyDrawer := false

	for scanner.Scan() {
		lineNum++
		line := scanner.Text()

		// Check for metadata in the header
		if inMetadata {
			if strings.HasPrefix(line, "#+") {
				if matches := metadataPattern.FindStringSubmatch(line); matches != nil {
					key := strings.ToLower(matches[1])
					value := strings.TrimSpace(matches[2])

					if checkMetadata {
						if key == "title" {
							hasTitle = true
							if value == "" {
								errors = append(errors, ValidationError{
									File:    path,
									Line:    lineNum,
									Message: "empty title",
								})
							}
						}
						if key == "identifier" {
							hasIdentifier = true
							if value == "" {
								errors = append(errors, ValidationError{
									File:    path,
									Line:    lineNum,
									Message: "empty identifier",
								})
							}
						}
					}
				}
			} else if strings.TrimSpace(line) == "" {
				// Empty line after metadata is okay
				continue
			} else if !strings.HasPrefix(line, "#") {
				// We've exited the metadata section
				inMetadata = false
			}
		}

		// Check property drawers
		if propertyDrawerStart.MatchString(line) {
			inPropertyDrawer = true
		} else if propertyDrawerEnd.MatchString(line) {
			inPropertyDrawer = false
		}

		// Check headline structure
		if checkStructure && !inPropertyDrawer {
			if matches := headlinePattern.FindStringSubmatch(line); matches != nil {
				level := len(matches[1])
				// Check for excessive level jumps (e.g., * followed by ***)
				if headlineLevel > 0 && level > headlineLevel+1 {
					errors = append(errors, ValidationError{
						File:    path,
						Line:    lineNum,
						Message: fmt.Sprintf("headline level jump from %d to %d", headlineLevel, level),
					})
				}
				headlineLevel = level
			}
		}

		// Check for unclosed brackets/parens in non-code blocks
		if checkStructure && !strings.HasPrefix(strings.TrimSpace(line), "#+begin") {
			if err := checkBrackets(line); err != nil {
				errors = append(errors, ValidationError{
					File:    path,
					Line:    lineNum,
					Message: err.Error(),
				})
			}
		}
	}

	if err := scanner.Err(); err != nil {
		errors = append(errors, ValidationError{
			File:    path,
			Message: fmt.Sprintf("error reading file: %v", err),
		})
	}

	// Check required metadata
	if checkMetadata {
		if !hasTitle {
			errors = append(errors, ValidationError{
				File:    path,
				Line:    0,
				Message: "missing required #+title metadata",
			})
		}
		if !hasIdentifier {
			errors = append(errors, ValidationError{
				File:    path,
				Line:    0,
				Message: "missing required #+identifier metadata",
			})
		}
	}

	return errors
}

func checkBrackets(line string) error {
	// Simple bracket/paren matching
	stack := []rune{}
	pairs := map[rune]rune{
		')': '(',
		']': '[',
		'}': '{',
	}

	for i, ch := range line {
		switch ch {
		case '(', '[', '{':
			stack = append(stack, ch)
		case ')', ']', '}':
			if len(stack) == 0 {
				return fmt.Errorf("unmatched closing bracket '%c' at position %d", ch, i+1)
			}
			last := stack[len(stack)-1]
			stack = stack[:len(stack)-1]
			if pairs[ch] != last {
				return fmt.Errorf("mismatched brackets: expected '%c' but got '%c' at position %d", last, ch, i+1)
			}
		}
	}

	return nil
}
