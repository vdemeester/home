# Deps Workflow

Manage Go module dependencies, updates, and security.

## When to Use

- "add go dependency"
- "update dependencies"
- "go mod tidy"
- "check for vulnerabilities"
- "vendor dependencies"

## Quick Commands

### Adding Dependencies
```bash
# Add latest version
go get github.com/pkg/errors

# Add specific version
go get github.com/pkg/errors@v0.9.1

# Add from specific commit
go get github.com/pkg/errors@commit-hash

# Add package and update go.mod
go get -u github.com/pkg/errors
```

### Updating Dependencies
```bash
# Update all dependencies
go get -u ./...

# Update specific dependency
go get -u github.com/pkg/errors

# Update to latest patch
go get -u=patch ./...

# Update direct dependencies only
go get -u -t ./...
```

### Module Maintenance
```bash
# Tidy modules (remove unused, add missing)
go mod tidy

# Verify dependencies
go mod verify

# Download dependencies
go mod download

# View dependency graph
go mod graph

# Explain why module is needed
go mod why github.com/pkg/errors
```

### Vendoring
```bash
# Create vendor directory
go mod vendor

# Verify vendor matches go.mod
go mod vendor -v

# Build using vendor
go build -mod=vendor

# List vendored packages
go list -m -json all
```

## Viewing Dependencies

### List Modules
```bash
# List all dependencies
go list -m all

# List direct dependencies only
go list -m -f '{{if not .Indirect}}{{.Path}}{{end}}' all

# List outdated dependencies
go list -u -m all

# Show module details
go list -m -json github.com/pkg/errors
```

### Dependency Tree
```bash
# Install go-mod-graph-chart
go install github.com/nikolaydubina/go-mod-graph-chart@latest

# Generate dependency graph
go mod graph | go-mod-graph-chart > deps.svg
```

## Security

### Vulnerability Scanning
```bash
# Install govulncheck
go install golang.org/x/vuln/cmd/govulncheck@latest

# Check for vulnerabilities
govulncheck ./...

# Check specific package
govulncheck -package github.com/pkg/errors ./...
```

### Update go.sum
```bash
# go.sum should be committed
git add go.sum

# If go.sum is out of sync
go mod tidy
```

## Dependency Replacement

### Local Replacement
```bash
# Replace with local version (for development)
go mod edit -replace github.com/pkg/errors=../local/errors

# Remove replacement
go mod edit -dropreplace github.com/pkg/errors
```

### Fork Replacement
```bash
# Replace with fork
go mod edit -replace github.com/original/pkg=github.com/yourfork/pkg@version
```

### go.mod Example
```go
module github.com/yourorg/yourproject

go 1.21

require (
    github.com/pkg/errors v0.9.1
    github.com/spf13/cobra v1.7.0
)

require (
    // Indirect dependencies
    github.com/spf13/pflag v1.0.5 // indirect
)

replace (
    // Local development
    github.com/internal/lib => ../lib
)
```

## Version Pinning

### Pin to Specific Version
```bash
# Use exact version
go get github.com/pkg/errors@v0.9.1

# Pin in go.mod
go mod edit -require=github.com/pkg/errors@v0.9.1
```

### Upgrade Constraints
```bash
# Latest version compatible with go.mod
go get -u github.com/pkg/errors

# Latest minor version
go get -u=patch github.com/pkg/errors
```

## Cleaning Up

### Remove Unused Dependencies
```bash
# Tidy removes unused
go mod tidy

# Check what will be removed
go mod tidy -v
```

### Clear Module Cache
```bash
# Clean entire cache
go clean -modcache

# Clean specific module
go clean -modcache -n
```

## Makefile Example

```makefile
.PHONY: deps
deps:
	go mod download
	go mod verify

.PHONY: deps-update
deps-update:
	go get -u ./...
	go mod tidy

.PHONY: deps-tidy
deps-tidy:
	go mod tidy
	go mod verify

.PHONY: deps-vendor
deps-vendor:
	go mod vendor

.PHONY: deps-vuln
deps-vuln:
	govulncheck ./...

.PHONY: deps-graph
deps-graph:
	go mod graph | go-mod-graph-chart > deps.svg
```

## Best Practices

1. **Always run `go mod tidy`**: After dependency changes
2. **Commit go.sum**: Essential for reproducibility
3. **Verify before deploying**: Run `go mod verify`
4. **Check vulnerabilities**: Use govulncheck regularly
5. **Use semantic versions**: Prefer tagged versions over commits
6. **Review updates**: Check changelogs before updating
7. **Vendor for critical projects**: Consider vendoring for stability
8. **Pin major versions**: Avoid unexpected breaking changes

## CI/CD Integration

### GitHub Actions
```yaml
- name: Download dependencies
  run: go mod download

- name: Verify dependencies
  run: go mod verify

- name: Check for vulnerabilities
  run: |
    go install golang.org/x/vuln/cmd/govulncheck@latest
    govulncheck ./...

- name: Check tidy
  run: |
    go mod tidy
    git diff --exit-code go.mod go.sum
```

## Troubleshooting

### Dependency Conflicts
```bash
# Show why dependency is needed
go mod why github.com/pkg/errors

# Check dependency graph
go mod graph | grep github.com/pkg/errors
```

### Checksum Mismatch
```bash
# Update checksums
go mod tidy

# Clear cache and retry
go clean -modcache
go mod download
```

## Resources

- [Go Modules Reference](https://go.dev/ref/mod)
- [govulncheck](https://pkg.go.dev/golang.org/x/vuln/cmd/govulncheck)
- [Go Module Wiki](https://github.com/golang/go/wiki/Modules)
