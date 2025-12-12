package cache

import (
	"os"
	"path/filepath"
	"testing"
	"time"
)

func TestCache(t *testing.T) {
	// Create temporary cache directory
	tmpDir := t.TempDir()

	c := &Cache{
		baseDir: tmpDir,
		ttl:     1 * time.Second,
	}

	t.Run("Set and Get", func(t *testing.T) {
		type testData struct {
			Name  string
			Value int
		}

		original := testData{Name: "test", Value: 42}

		if err := c.Set("test-key", original); err != nil {
			t.Fatalf("Set failed: %v", err)
		}

		var retrieved testData
		if err := c.Get("test-key", &retrieved); err != nil {
			t.Fatalf("Get failed: %v", err)
		}

		if retrieved.Name != original.Name || retrieved.Value != original.Value {
			t.Errorf("Retrieved data mismatch: got %+v, want %+v", retrieved, original)
		}
	})

	t.Run("Get non-existent key", func(t *testing.T) {
		var data string
		if err := c.Get("non-existent", &data); err != nil {
			t.Fatalf("Get should not error on non-existent key: %v", err)
		}
		if data != "" {
			t.Errorf("Expected empty data for non-existent key, got: %s", data)
		}
	})

	t.Run("Expiration", func(t *testing.T) {
		if err := c.Set("expire-test", "value"); err != nil {
			t.Fatalf("Set failed: %v", err)
		}

		// Wait for expiration
		time.Sleep(2 * time.Second)

		var data string
		if err := c.Get("expire-test", &data); err != nil {
			t.Fatalf("Get failed: %v", err)
		}

		if data != "" {
			t.Errorf("Expected empty data after expiration, got: %s", data)
		}

		// Verify file was cleaned up
		filePath := filepath.Join(tmpDir, "expire-test.json")
		if _, err := os.Stat(filePath); !os.IsNotExist(err) {
			t.Error("Expected cache file to be deleted after expiration")
		}
	})

	t.Run("Delete", func(t *testing.T) {
		if err := c.Set("delete-test", "value"); err != nil {
			t.Fatalf("Set failed: %v", err)
		}

		if err := c.Delete("delete-test"); err != nil {
			t.Fatalf("Delete failed: %v", err)
		}

		var data string
		if err := c.Get("delete-test", &data); err != nil {
			t.Fatalf("Get failed: %v", err)
		}

		if data != "" {
			t.Errorf("Expected empty data after delete, got: %s", data)
		}
	})

	t.Run("Clear", func(t *testing.T) {
		// Set multiple entries
		for i := 0; i < 3; i++ {
			key := filepath.Join("clear-test", string(rune('a'+i)))
			if err := c.Set(key, i); err != nil {
				t.Fatalf("Set failed: %v", err)
			}
		}

		if err := c.Clear(); err != nil {
			t.Fatalf("Clear failed: %v", err)
		}

		// Verify all entries are gone
		entries, err := os.ReadDir(tmpDir)
		if err != nil {
			t.Fatalf("ReadDir failed: %v", err)
		}

		if len(entries) != 0 {
			t.Errorf("Expected 0 cache entries after clear, got %d", len(entries))
		}
	})
}
