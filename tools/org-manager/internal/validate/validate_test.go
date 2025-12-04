package validate

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCheckBrackets(t *testing.T) {
	tests := []struct {
		name    string
		line    string
		wantErr bool
	}{
		{
			name:    "balanced parentheses",
			line:    "This is a (test) line",
			wantErr: false,
		},
		{
			name:    "balanced brackets",
			line:    "Link to [[file:test.org][test]]",
			wantErr: false,
		},
		{
			name:    "balanced braces",
			line:    "Some code {foo: bar}",
			wantErr: false,
		},
		{
			name:    "nested balanced",
			line:    "Nested ((test [with {brackets}]))",
			wantErr: false,
		},
		{
			name:    "unmatched opening",
			line:    "Unmatched (test",
			wantErr: false, // We don't check for unclosed brackets, only mismatched closes
		},
		{
			name:    "unmatched closing",
			line:    "Unmatched test)",
			wantErr: true,
		},
		{
			name:    "mismatched brackets",
			line:    "Mismatched (test]",
			wantErr: true,
		},
		{
			name:    "empty line",
			line:    "",
			wantErr: false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := checkBrackets(tt.line)
			if (err != nil) != tt.wantErr {
				t.Errorf("checkBrackets(%q) error = %v, wantErr %v", tt.line, err, tt.wantErr)
			}
		})
	}
}

func TestValidateOrgFile(t *testing.T) {
	// Create a temporary directory for test files
	tmpDir, err := os.MkdirTemp("", "org-manager-test-*")
	if err != nil {
		t.Fatalf("failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	tests := []struct {
		name           string
		content        string
		checkMetadata  bool
		checkStructure bool
		wantErrors     int
	}{
		{
			name: "valid org file",
			content: `#+title: Test Note
#+identifier: 20231028T082123

* Headline 1
** Headline 2
`,
			checkMetadata:  true,
			checkStructure: true,
			wantErrors:     0,
		},
		{
			name: "missing title",
			content: `#+identifier: 20231028T082123

* Content
`,
			checkMetadata:  true,
			checkStructure: false,
			wantErrors:     1, // missing title
		},
		{
			name: "missing identifier",
			content: `#+title: Test

* Content
`,
			checkMetadata:  true,
			checkStructure: false,
			wantErrors:     1, // missing identifier
		},
		{
			name: "headline level jump",
			content: `#+title: Test
#+identifier: 20231028T082123

* Level 1
*** Level 3 (skipped 2)
`,
			checkMetadata:  false,
			checkStructure: true,
			wantErrors:     1, // level jump
		},
		{
			name: "empty title",
			content: `#+title:
#+identifier: 20231028T082123

* Content
`,
			checkMetadata:  true,
			checkStructure: false,
			wantErrors:     1, // empty title
		},
		{
			name: "unmatched brackets",
			content: `#+title: Test
#+identifier: 20231028T082123

* Headline with unclosed (bracket
`,
			checkMetadata:  false,
			checkStructure: true,
			wantErrors:     0, // We don't check for unclosed brackets currently
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			// Create test file
			testFile := filepath.Join(tmpDir, "test.org")
			if err := os.WriteFile(testFile, []byte(tt.content), 0644); err != nil {
				t.Fatalf("failed to create test file: %v", err)
			}

			errors := validateOrgFile(testFile, tt.checkMetadata, tt.checkStructure)

			if len(errors) != tt.wantErrors {
				t.Errorf("validateOrgFile() got %d errors, want %d", len(errors), tt.wantErrors)
				for _, e := range errors {
					t.Logf("  Error: %s", e.String())
				}
			}
		})
	}
}

func TestValidationErrorString(t *testing.T) {
	tests := []struct {
		name string
		err  ValidationError
		want string
	}{
		{
			name: "with line and column",
			err: ValidationError{
				File:    "/path/to/file.org",
				Line:    10,
				Column:  5,
				Message: "test error",
			},
			want: "/path/to/file.org:10:5: test error",
		},
		{
			name: "without line",
			err: ValidationError{
				File:    "/path/to/file.org",
				Message: "test error",
			},
			want: "/path/to/file.org: test error",
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			got := tt.err.String()
			if got != tt.want {
				t.Errorf("ValidationError.String() = %q, want %q", got, tt.want)
			}
		})
	}
}
