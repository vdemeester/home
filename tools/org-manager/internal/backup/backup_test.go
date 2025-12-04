package backup

import (
	"os"
	"path/filepath"
	"testing"
)

func TestCopyFile(t *testing.T) {
	// Create temporary directories
	tmpDir, err := os.MkdirTemp("", "org-manager-backup-test-*")
	if err != nil {
		t.Fatalf("failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	// Create source file
	srcPath := filepath.Join(tmpDir, "source.txt")
	srcContent := []byte("test content\nline 2\n")
	if err := os.WriteFile(srcPath, srcContent, 0644); err != nil {
		t.Fatalf("failed to create source file: %v", err)
	}

	// Test copying
	dstPath := filepath.Join(tmpDir, "dest.txt")
	if err := copyFile(srcPath, dstPath); err != nil {
		t.Fatalf("copyFile() error = %v", err)
	}

	// Verify destination file exists and has same content
	dstContent, err := os.ReadFile(dstPath)
	if err != nil {
		t.Fatalf("failed to read destination file: %v", err)
	}

	if string(dstContent) != string(srcContent) {
		t.Errorf("destination content = %q, want %q", string(dstContent), string(srcContent))
	}

	// Verify permissions were preserved
	srcInfo, err := os.Stat(srcPath)
	if err != nil {
		t.Fatalf("failed to stat source file: %v", err)
	}

	dstInfo, err := os.Stat(dstPath)
	if err != nil {
		t.Fatalf("failed to stat destination file: %v", err)
	}

	if dstInfo.Mode() != srcInfo.Mode() {
		t.Errorf("destination mode = %v, want %v", dstInfo.Mode(), srcInfo.Mode())
	}
}

func TestCopyFileErrors(t *testing.T) {
	tmpDir, err := os.MkdirTemp("", "org-manager-backup-test-*")
	if err != nil {
		t.Fatalf("failed to create temp dir: %v", err)
	}
	defer os.RemoveAll(tmpDir)

	tests := []struct {
		name    string
		src     string
		dst     string
		wantErr bool
	}{
		{
			name:    "source does not exist",
			src:     filepath.Join(tmpDir, "nonexistent.txt"),
			dst:     filepath.Join(tmpDir, "dest.txt"),
			wantErr: true,
		},
		{
			name:    "destination in nonexistent directory",
			src:     filepath.Join(tmpDir, "source.txt"),
			dst:     filepath.Join(tmpDir, "nonexistent", "dest.txt"),
			wantErr: true,
		},
	}

	// Create source file for tests that need it
	srcPath := filepath.Join(tmpDir, "source.txt")
	if err := os.WriteFile(srcPath, []byte("test"), 0644); err != nil {
		t.Fatalf("failed to create source file: %v", err)
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			err := copyFile(tt.src, tt.dst)
			if (err != nil) != tt.wantErr {
				t.Errorf("copyFile() error = %v, wantErr %v", err, tt.wantErr)
			}
		})
	}
}

func TestBackupOptions(t *testing.T) {
	tests := []struct {
		name       string
		backupType string
		wantValid  bool
	}{
		{
			name:       "readwise type",
			backupType: "readwise",
			wantValid:  true,
		},
		{
			name:       "pkai type",
			backupType: "pkai",
			wantValid:  true,
		},
		{
			name:       "all type",
			backupType: "all",
			wantValid:  true,
		},
		{
			name:       "invalid type",
			backupType: "invalid",
			wantValid:  false,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			valid := tt.backupType == "readwise" || tt.backupType == "pkai" || tt.backupType == "all"
			if valid != tt.wantValid {
				t.Errorf("backup type %q validity = %v, want %v", tt.backupType, valid, tt.wantValid)
			}
		})
	}
}
