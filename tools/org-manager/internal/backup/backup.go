// Package backup provides functionality for backing up org-mode files.
package backup

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/spf13/cobra"
)

// Options holds configuration for backup operations.
type Options struct {
	BackupType string
	Dest       string
	Compress   bool
	Timestamp  bool
}

// NewCommand creates the backup subcommand.
func NewCommand(orgDir *string) *cobra.Command {
	opts := &Options{}

	cmd := &cobra.Command{
		Use:   "backup",
		Short: "Backup readwise or pkai org notes",
		Long: `Backup org notes to a specified destination.

This command can backup:
- readwise notes (files containing ==readwise=)
- pkai notes (files containing ==pkai--)
- all notes of both types

By default, a timestamp is added to the backup directory name.`,
		Example: `  # Backup all readwise notes
  org-manager backup --type readwise --dest ~/backups

  # Backup pkai notes without timestamp
  org-manager backup --type pkai --dest ~/backups/pkai --timestamp=false

  # Backup all notes
  org-manager backup --type all --dest ~/backups`,
		RunE: func(cmd *cobra.Command, args []string) error {
			return Run(opts, *orgDir)
		},
	}

	cmd.Flags().StringVar(&opts.BackupType, "type", "all", "Type of notes to backup: readwise, pkai, or all")
	cmd.Flags().StringVar(&opts.Dest, "dest", "", "Destination directory for backup (required)")
	cmd.Flags().BoolVar(&opts.Compress, "compress", false, "Create a compressed tar.gz archive")
	cmd.Flags().BoolVar(&opts.Timestamp, "timestamp", true, "Add timestamp to backup directory/archive name")

	cmd.MarkFlagRequired("dest")

	return cmd
}

// Run executes the backup operation.
func Run(opts *Options, orgDir string) error {
	if opts.BackupType != "readwise" && opts.BackupType != "pkai" && opts.BackupType != "all" {
		return fmt.Errorf("invalid backup type: %s (must be readwise, pkai, or all)", opts.BackupType)
	}

	notesDir := filepath.Join(orgDir, "notes")
	if _, err := os.Stat(notesDir); os.IsNotExist(err) {
		return fmt.Errorf("notes directory does not exist: %s", notesDir)
	}

	// Prepare destination
	destPath := opts.Dest
	if opts.Timestamp {
		ts := time.Now().Format("20060102-150405")
		if opts.Compress {
			destPath = filepath.Join(opts.Dest, fmt.Sprintf("org-backup-%s.tar.gz", ts))
		} else {
			destPath = filepath.Join(opts.Dest, fmt.Sprintf("org-backup-%s", ts))
		}
	}

	// Create destination directory if not compressing
	if !opts.Compress {
		if err := os.MkdirAll(destPath, 0755); err != nil {
			return fmt.Errorf("failed to create destination directory: %w", err)
		}
	} else {
		// Ensure parent directory exists for compressed archive
		parentDir := filepath.Dir(destPath)
		if err := os.MkdirAll(parentDir, 0755); err != nil {
			return fmt.Errorf("failed to create parent directory: %w", err)
		}
	}

	// Find files to backup
	var files []string
	err := filepath.Walk(notesDir, func(path string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return nil
		}
		if !strings.HasSuffix(path, ".org") {
			return nil
		}

		basename := filepath.Base(path)
		shouldBackup := false

		switch opts.BackupType {
		case "readwise":
			shouldBackup = strings.Contains(basename, "==readwise=")
		case "pkai":
			shouldBackup = strings.Contains(basename, "==pkai--")
		case "all":
			shouldBackup = strings.Contains(basename, "==readwise=") || strings.Contains(basename, "==pkai--")
		}

		if shouldBackup {
			files = append(files, path)
		}
		return nil
	})
	if err != nil {
		return fmt.Errorf("failed to scan notes directory: %w", err)
	}

	if len(files) == 0 {
		fmt.Printf("No files found to backup (type: %s)\n", opts.BackupType)
		return nil
	}

	// Perform backup
	if opts.Compress {
		return createTarGzArchive(files, notesDir, destPath)
	}
	return copyFiles(files, notesDir, destPath)
}

func copyFiles(files []string, sourceBase, destBase string) error {
	copied := 0
	for _, srcPath := range files {
		relPath, err := filepath.Rel(sourceBase, srcPath)
		if err != nil {
			return fmt.Errorf("failed to get relative path for %s: %w", srcPath, err)
		}

		destPath := filepath.Join(destBase, relPath)
		destDir := filepath.Dir(destPath)

		if err := os.MkdirAll(destDir, 0755); err != nil {
			return fmt.Errorf("failed to create directory %s: %w", destDir, err)
		}

		if err := copyFile(srcPath, destPath); err != nil {
			return fmt.Errorf("failed to copy %s: %w", srcPath, err)
		}
		copied++
	}

	fmt.Printf("Successfully backed up %d files to %s\n", copied, destBase)
	return nil
}

func copyFile(src, dst string) error {
	sourceFile, err := os.Open(src)
	if err != nil {
		return fmt.Errorf("opening source file: %w", err)
	}
	defer sourceFile.Close()

	destFile, err := os.Create(dst)
	if err != nil {
		return fmt.Errorf("creating destination file: %w", err)
	}

	// Copy file contents
	if _, err := io.Copy(destFile, sourceFile); err != nil {
		destFile.Close()
		return fmt.Errorf("copying file contents: %w", err)
	}

	// Close destination file and check for errors
	if err := destFile.Close(); err != nil {
		return fmt.Errorf("closing destination file: %w", err)
	}

	// Preserve permissions
	srcInfo, err := os.Stat(src)
	if err != nil {
		return fmt.Errorf("getting source file info: %w", err)
	}

	if err := os.Chmod(dst, srcInfo.Mode()); err != nil {
		return fmt.Errorf("setting permissions: %w", err)
	}

	return nil
}

func createTarGzArchive(files []string, sourceBase, archivePath string) error {
	// For now, we'll use the tar command rather than implementing tar.gz from scratch
	// This is simpler and more reliable
	return fmt.Errorf("compressed backup not yet implemented - use --compress=false for now")
}
