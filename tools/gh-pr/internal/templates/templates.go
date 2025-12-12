package templates

import (
	"crypto/sha256"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/vdemeester/home/tools/gh-pr/internal/cache"
)

// Template represents a PR template file
type Template struct {
	Path    string
	Name    string
	Content string
}

// Finder finds and caches PR templates
type Finder struct {
	cache *cache.Cache
}

// NewFinder creates a new template finder
func NewFinder() (*Finder, error) {
	c, err := cache.New(cache.DefaultTTL)
	if err != nil {
		return nil, fmt.Errorf("failed to create cache: %w", err)
	}

	return &Finder{
		cache: c,
	}, nil
}

// Find locates all PR templates in the repository
// If refresh is true, bypasses cache and performs fresh search
func (f *Finder) Find(refresh bool) ([]Template, error) {
	// Generate cache key based on current directory
	cwd, err := os.Getwd()
	if err != nil {
		return nil, err
	}

	cacheKey := f.generateCacheKey(cwd)

	// Try cache first unless refresh is requested
	if !refresh {
		var cached []Template
		if err := f.cache.Get(cacheKey, &cached); err == nil && cached != nil {
			return cached, nil
		}
	}

	// Search for templates
	templates, err := f.searchTemplates()
	if err != nil {
		return nil, err
	}

	// Cache the results
	if err := f.cache.Set(cacheKey, templates); err != nil {
		// Don't fail if caching fails, just log and continue
		fmt.Fprintf(os.Stderr, "Warning: failed to cache templates: %v\n", err)
	}

	return templates, nil
}

// ClearCache removes cached template information
func (f *Finder) ClearCache() error {
	return f.cache.Clear()
}

// generateCacheKey creates a unique cache key for the current repository
func (f *Finder) generateCacheKey(dir string) string {
	h := sha256.New()
	h.Write([]byte(dir))
	h.Write([]byte(time.Now().Format("2006-01-02"))) // Include date for daily refresh
	return fmt.Sprintf("templates-%x", h.Sum(nil))
}

// searchTemplates performs the actual search for PR templates
func (f *Finder) searchTemplates() ([]Template, error) {
	var templates []Template

	// Common locations for PR templates
	locations := []string{
		".github/PULL_REQUEST_TEMPLATE.md",
		".github/pull_request_template.md",
		".github/PULL_REQUEST_TEMPLATE/",
		"docs/PULL_REQUEST_TEMPLATE.md",
		"docs/pull_request_template.md",
	}

	for _, loc := range locations {
		info, err := os.Stat(loc)
		if err != nil {
			continue
		}

		if info.IsDir() {
			// List all markdown files in the directory
			entries, err := os.ReadDir(loc)
			if err != nil {
				continue
			}

			for _, entry := range entries {
				if entry.IsDir() {
					continue
				}

				name := entry.Name()
				if !strings.HasSuffix(name, ".md") {
					continue
				}

				path := filepath.Join(loc, name)
				content, err := os.ReadFile(path)
				if err != nil {
					continue
				}

				templates = append(templates, Template{
					Path:    path,
					Name:    strings.TrimSuffix(name, ".md"),
					Content: string(content),
				})
			}
		} else {
			content, err := os.ReadFile(loc)
			if err != nil {
				continue
			}

			// Extract name from path
			name := filepath.Base(loc)
			name = strings.TrimSuffix(name, ".md")
			if name == "PULL_REQUEST_TEMPLATE" || name == "pull_request_template" {
				name = "default"
			}

			templates = append(templates, Template{
				Path:    loc,
				Name:    name,
				Content: string(content),
			})
		}
	}

	return templates, nil
}

// ReadTemplate reads a specific template file
func ReadTemplate(path string) (string, error) {
	content, err := os.ReadFile(path)
	if err != nil {
		return "", fmt.Errorf("failed to read template %s: %w", path, err)
	}
	return string(content), nil
}
