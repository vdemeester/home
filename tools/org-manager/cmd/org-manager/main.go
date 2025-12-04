// Package main implements org-manager, a tool for managing org-mode files.
//
// org-manager provides functionality for backing up, validating, and checking
// links in org-mode files, with special support for readwise and pkai notes.
package main

import (
	"fmt"
	"os"
	"path/filepath"

	"github.com/spf13/cobra"
	"github.com/vdemeester/home/tools/org-manager/internal/backup"
	"github.com/vdemeester/home/tools/org-manager/internal/links"
	"github.com/vdemeester/home/tools/org-manager/internal/readwise"
	"github.com/vdemeester/home/tools/org-manager/internal/validate"
)

var (
	version = "0.1.0"
	orgDir  string
)

func main() {
	rootCmd := &cobra.Command{
		Use:   "org-manager",
		Short: "Manage and validate org-mode files",
		Long: `org-manager is a tool for managing org-mode files including:
- Backing up readwise and pkai notes
- Validating org-mode file structure and metadata
- Checking links (local, denote, etc.)
- Integrating with go-org-readwise`,
		Version: version,
	}

	// Global flags
	defaultOrgDir := filepath.Join(os.Getenv("HOME"), "desktop", "org")
	rootCmd.PersistentFlags().StringVar(&orgDir, "org-dir", defaultOrgDir, "Path to org directory")

	// Add subcommands
	rootCmd.AddCommand(backup.NewCommand(&orgDir))
	rootCmd.AddCommand(validate.NewCommand(&orgDir))
	rootCmd.AddCommand(links.NewCommand(&orgDir))
	rootCmd.AddCommand(readwise.NewCommand(&orgDir))

	if err := rootCmd.Execute(); err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(1)
	}
}
