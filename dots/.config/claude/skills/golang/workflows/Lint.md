# Lint Workflow

Check code quality, formatting, and common mistakes with Go linting tools.

## When to Use

- "lint go code"
- "format go files"
- "check code quality"
- "run gofmt"
- "run golangci-lint"

## Quick Commands

### Formatting
```bash
# Format all Go files
gofmt -w .

# Check formatting (don't write)
gofmt -d .

# List files needing formatting
gofmt -l .

# Format and organize imports
goimports -w .
```

### Go Vet
```bash
# Vet current package
go vet

# Vet all packages
go vet ./...

# Vet specific package
go vet ./internal/handlers
```

### golangci-lint
```bash
# Install
go install github.com/golangci/golangci-lint/cmd/golangci-lint@latest

# Run all linters
golangci-lint run

# Auto-fix issues
golangci-lint run --fix

# Run specific linters
golangci-lint run --enable=gosec,exhaustive

# Fast mode (skip cached)
golangci-lint run --fast
```

## Common Linters

### Static Analysis
- **staticcheck**: Comprehensive static analysis
- **gosec**: Security issues
- **errcheck**: Unchecked errors
- **govet**: Official Go vet
- **ineffassign**: Ineffectual assignments

### Code Quality
- **gocyclo**: Cyclomatic complexity
- **gocognit**: Cognitive complexity
- **dupl**: Code duplication
- **goconst**: Repeated string constants
- **unconvert**: Unnecessary type conversions

### Style
- **gofmt**: Code formatting
- **goimports**: Import organization
- **revive**: Flexible linter (golint replacement)
- **stylecheck**: Style recommendations

## Configuration

### .golangci.yml Example
```yaml
run:
  timeout: 5m
  tests: true

linters:
  enable:
    - gofmt
    - goimports
    - govet
    - staticcheck
    - errcheck
    - gosec
    - ineffassign
    - unconvert
    - goconst
    - revive

linters-settings:
  goimports:
    local-prefixes: github.com/yourorg/yourrepo

  govet:
    check-shadowing: true

  revive:
    rules:
      - name: exported
        disabled: true

issues:
  exclude-use-default: false
  max-issues-per-linter: 0
  max-same-issues: 0
```

## Common Fixes

### Unused Variables
```go
// Bad
func example() {
    x := 5
    // x never used
}

// Good
func example() {
    _ = 5  // Explicitly unused
}
```

### Error Checking
```go
// Bad
file, _ := os.Open("file.txt")

// Good
file, err := os.Open("file.txt")
if err != nil {
    return err
}
defer file.Close()
```

### Import Organization
```go
// Bad
import (
    "github.com/external/pkg"
    "fmt"
    "myproject/internal"
)

// Good (goimports fixes this)
import (
    "fmt"

    "github.com/external/pkg"

    "myproject/internal"
)
```

## Makefile Example

```makefile
.PHONY: fmt
fmt:
	gofmt -w .
	goimports -w .

.PHONY: lint
lint:
	golangci-lint run

.PHONY: lint-fix
lint-fix:
	golangci-lint run --fix

.PHONY: vet
vet:
	go vet ./...

.PHONY: check
check: fmt vet lint
	@echo "All checks passed!"
```

## CI/CD Integration

### GitHub Actions
```yaml
- name: Format check
  run: |
    if [ "$(gofmt -l . | wc -l)" -gt 0 ]; then
      echo "Files need formatting:"
      gofmt -l .
      exit 1
    fi

- name: golangci-lint
  uses: golangci/golangci-lint-action@v3
  with:
    version: latest
```

## Best Practices

1. **Run before committing**: Add to pre-commit hooks
2. **Use goimports over gofmt**: Manages imports too
3. **Enable gosec**: Catch security issues
4. **Fix formatting first**: Then tackle other issues
5. **Configure for your team**: Use .golangci.yml
6. **Run in CI**: Ensure consistency
7. **Auto-fix when safe**: Use `--fix` flag

## Resources

- [gofmt](https://pkg.go.dev/cmd/gofmt)
- [go vet](https://pkg.go.dev/cmd/vet)
- [golangci-lint](https://golangci-lint.run/)
- [staticcheck](https://staticcheck.io/)
