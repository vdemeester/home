// Package readwise provides integration with go-org-readwise.
package readwise

import (
	"fmt"
	"os"
	"os/exec"

	"github.com/spf13/cobra"
)

// Options holds configuration for readwise operations.
type Options struct {
	Sync    bool
	DryRun  bool
	Verbose bool
}

// NewCommand creates the readwise subcommand.
func NewCommand(orgDir *string) *cobra.Command {
	opts := &Options{}

	cmd := &cobra.Command{
		Use:   "readwise",
		Short: "Call go-org-readwise to sync highlights",
		Long: `Wrapper around go-org-readwise for syncing Readwise highlights to org files.

This command calls the go-org-readwise tool which should be installed
separately. It syncs your Readwise highlights into org-mode files in
the configured org directory.`,
		Example: `  # Sync readwise highlights
  org-manager readwise --sync

  # Dry run to see what would be synced
  org-manager readwise --sync --dry-run

  # Verbose output
  org-manager readwise --sync --verbose`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return Run(opts, *orgDir)
		},
	}

	cmd.Flags().BoolVar(&opts.Sync, "sync", false, "Sync readwise highlights")
	cmd.Flags().BoolVar(&opts.DryRun, "dry-run", false, "Show what would be done without making changes")
	cmd.Flags().BoolVarP(&opts.Verbose, "verbose", "v", false, "Show verbose output")

	return cmd
}

// Run executes the readwise sync operation.
func Run(opts *Options, orgDir string) error {
	_, err := exec.LookPath("go-org-readwise")
	if err != nil {
		return fmt.Errorf("go-org-readwise not found in PATH. Please install it first")
	}

	if !opts.Sync {
		return fmt.Errorf("please specify --sync to sync readwise highlights")
	}

	args := []string{}

	if opts.DryRun {
		args = append(args, "--dry-run")
	}

	if opts.Verbose {
		args = append(args, "--verbose")
	}

	args = append(args, "--org-dir", orgDir)

	cmdExec := exec.Command("go-org-readwise", args...)
	cmdExec.Stdout = os.Stdout
	cmdExec.Stderr = os.Stderr
	cmdExec.Stdin = os.Stdin

	if opts.Verbose {
		fmt.Printf("Executing: go-org-readwise %s\n", args)
	}

	if err := cmdExec.Run(); err != nil {
		return fmt.Errorf("go-org-readwise failed: %w", err)
	}

	fmt.Println("Readwise sync completed successfully")
	return nil
}
