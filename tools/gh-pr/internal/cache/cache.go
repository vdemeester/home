package cache

import (
	"encoding/json"
	"os"
	"path/filepath"
	"time"
)

const (
	// DefaultTTL is the default time-to-live for cache entries (1 week)
	DefaultTTL = 7 * 24 * time.Hour

	// CacheDir is the directory where cache files are stored
	cacheDir = ".cache/gh-pr"
)

// Entry represents a cached item with expiration
type Entry struct {
	Data      interface{} `json:"data"`
	ExpiresAt time.Time   `json:"expires_at"`
}

// Cache handles caching of data with TTL support
type Cache struct {
	baseDir string
	ttl     time.Duration
}

// New creates a new Cache instance
func New(ttl time.Duration) (*Cache, error) {
	homeDir, err := os.UserHomeDir()
	if err != nil {
		return nil, err
	}

	baseDir := filepath.Join(homeDir, cacheDir)
	if err := os.MkdirAll(baseDir, 0755); err != nil {
		return nil, err
	}

	if ttl == 0 {
		ttl = DefaultTTL
	}

	return &Cache{
		baseDir: baseDir,
		ttl:     ttl,
	}, nil
}

// Get retrieves a value from cache
// Returns nil if not found or expired
func (c *Cache) Get(key string, dest interface{}) error {
	filePath := filepath.Join(c.baseDir, key+".json")

	data, err := os.ReadFile(filePath)
	if err != nil {
		if os.IsNotExist(err) {
			return nil
		}
		return err
	}

	var entry Entry
	if err := json.Unmarshal(data, &entry); err != nil {
		return err
	}

	// Check if expired
	if time.Now().After(entry.ExpiresAt) {
		// Clean up expired entry
		_ = os.Remove(filePath)
		return nil
	}

	// Unmarshal the data into the destination
	dataBytes, err := json.Marshal(entry.Data)
	if err != nil {
		return err
	}

	return json.Unmarshal(dataBytes, dest)
}

// Set stores a value in cache with the configured TTL
func (c *Cache) Set(key string, value interface{}) error {
	entry := Entry{
		Data:      value,
		ExpiresAt: time.Now().Add(c.ttl),
	}

	data, err := json.Marshal(entry)
	if err != nil {
		return err
	}

	filePath := filepath.Join(c.baseDir, key+".json")
	return os.WriteFile(filePath, data, 0644)
}

// Delete removes an entry from cache
func (c *Cache) Delete(key string) error {
	filePath := filepath.Join(c.baseDir, key+".json")
	err := os.Remove(filePath)
	if os.IsNotExist(err) {
		return nil
	}
	return err
}

// Clear removes all cache entries
func (c *Cache) Clear() error {
	entries, err := os.ReadDir(c.baseDir)
	if err != nil {
		return err
	}

	for _, entry := range entries {
		if !entry.IsDir() {
			filePath := filepath.Join(c.baseDir, entry.Name())
			if err := os.Remove(filePath); err != nil {
				return err
			}
		}
	}

	return nil
}
