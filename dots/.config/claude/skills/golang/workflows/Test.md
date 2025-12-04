# Test Workflow

Run Go tests with coverage, race detection, and various testing patterns.

## When to Use

- "run go tests"
- "test go code"
- "check test coverage"
- "run specific test"
- "test with race detector"

## Quick Commands

### Basic Testing
```bash
# Run all tests in current package
go test

# Run tests in all packages
go test ./...

# Verbose output
go test -v ./...

# Run specific test
go test -run TestMyFunction

# Run tests matching pattern
go test -run TestUser.*
```

### Test Coverage
```bash
# Run tests with coverage
go test -cover ./...

# Generate coverage profile
go test -coverprofile=coverage.out ./...

# View coverage in browser
go tool cover -html=coverage.out

# Show coverage by function
go tool cover -func=coverage.out

# Coverage for specific package
go test -cover ./internal/auth
```

### Race Detection
```bash
# Run tests with race detector
go test -race ./...

# Race detection for specific package
go test -race ./internal/handlers
```

### Performance
```bash
# Run short tests only (skip long-running tests)
go test -short ./...

# Set timeout
go test -timeout 30s ./...

# Run tests in parallel
go test -parallel 4 ./...
```

## Test Patterns

### Table-Driven Tests
```go
func TestValidateEmail(t *testing.T) {
    tests := []struct {
        name    string
        email   string
        wantErr bool
    }{
        {
            name:    "valid email",
            email:   "user@example.com",
            wantErr: false,
        },
        {
            name:    "missing @",
            email:   "userexample.com",
            wantErr: true,
        },
        {
            name:    "empty email",
            email:   "",
            wantErr: true,
        },
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := validateEmail(tt.email)
            if (err != nil) != tt.wantErr {
                t.Errorf("validateEmail(%q) error = %v, wantErr %v",
                    tt.email, err, tt.wantErr)
            }
        })
    }
}
```

### Test Helpers
```go
// Mark as helper to get better error line numbers
func assertEqual(t *testing.T, got, want int) {
    t.Helper()
    if got != want {
        t.Errorf("got %d, want %d", got, want)
    }
}

func TestCalculation(t *testing.T) {
    result := calculate(2, 3)
    assertEqual(t, result, 5)
}
```

### Setup and Teardown
```go
func TestMain(m *testing.M) {
    // Setup
    setup()

    // Run tests
    code := m.Run()

    // Teardown
    teardown()

    os.Exit(code)
}

func TestWithCleanup(t *testing.T) {
    // Setup
    db := setupTestDB(t)

    // Cleanup automatically called after test
    t.Cleanup(func() {
        db.Close()
    })

    // Test code
}
```

### Subtests
```go
func TestUserOperations(t *testing.T) {
    t.Run("Create", func(t *testing.T) {
        // Test user creation
    })

    t.Run("Update", func(t *testing.T) {
        // Test user update
    })

    t.Run("Delete", func(t *testing.T) {
        // Test user deletion
    })
}
```

## Test Organization

### Package Structures
```go
// White-box testing (same package)
package mypackage

func TestInternalFunction(t *testing.T) {
    // Can access private functions
}

// Black-box testing (separate package)
package mypackage_test

import "myproject/mypackage"

func TestPublicAPI(t *testing.T) {
    // Only access exported functions
}
```

### Test Files
```
mypackage/
├── user.go
├── user_test.go       # Tests for user.go
├── auth.go
└── auth_test.go       # Tests for auth.go
```

## Advanced Testing

### Testing with Timeouts
```go
func TestWithTimeout(t *testing.T) {
    ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
    defer cancel()

    done := make(chan bool)
    go func() {
        // Long running operation
        done <- true
    }()

    select {
    case <-done:
        // Success
    case <-ctx.Done():
        t.Fatal("test timed out")
    }
}
```

### Testing HTTP Handlers
```go
func TestHandler(t *testing.T) {
    req := httptest.NewRequest("GET", "/users", nil)
    w := httptest.NewRecorder()

    handler(w, req)

    if w.Code != http.StatusOK {
        t.Errorf("got status %d, want %d", w.Code, http.StatusOK)
    }
}
```

### Mocking with Interfaces
```go
// Define interface
type DataStore interface {
    Get(id string) (*User, error)
}

// Mock implementation
type MockDataStore struct {
    GetFunc func(id string) (*User, error)
}

func (m *MockDataStore) Get(id string) (*User, error) {
    return m.GetFunc(id)
}

// Use in tests
func TestService(t *testing.T) {
    mock := &MockDataStore{
        GetFunc: func(id string) (*User, error) {
            return &User{ID: id, Name: "Test"}, nil
        },
    }

    service := NewService(mock)
    // Test service...
}
```

## Coverage Analysis

### Coverage Modes
```bash
# Set coverage mode (default: set)
go test -covermode=set -coverprofile=coverage.out ./...

# Count mode (how many times each statement runs)
go test -covermode=count -coverprofile=coverage.out ./...

# Atomic mode (for parallel tests)
go test -covermode=atomic -coverprofile=coverage.out ./...
```

### Coverage Thresholds
```bash
# Check if coverage meets threshold
go test -cover ./... | grep -E "coverage: [0-9]+\.[0-9]+%" | \
    awk '{if ($2 < 80.0) exit 1}'
```

### Combine Coverage from Multiple Packages
```bash
# Generate coverage for each package
go test -coverprofile=coverage.out -covermode=atomic ./...

# View total coverage
go tool cover -func=coverage.out | grep total
```

## Makefile Example

```makefile
.PHONY: test
test:
	go test -v ./...

.PHONY: test-coverage
test-coverage:
	go test -coverprofile=coverage.out -covermode=atomic ./...
	go tool cover -html=coverage.out -o coverage.html
	@echo "Coverage report generated: coverage.html"

.PHONY: test-race
test-race:
	go test -race -short ./...

.PHONY: test-all
test-all: test-race test-coverage
	@echo "All tests passed with race detection and coverage"

.PHONY: test-clean
test-clean:
	rm -f coverage.out coverage.html
```

## CI/CD Integration

### GitHub Actions Example
```yaml
- name: Run tests
  run: go test -v ./...

- name: Run tests with coverage
  run: go test -coverprofile=coverage.out -covermode=atomic ./...

- name: Upload coverage
  uses: codecov/codecov-action@v3
  with:
    files: ./coverage.out

- name: Run tests with race detector
  run: go test -race ./...
```

## Test Flags Reference

| Flag | Purpose |
|------|---------|
| `-v` | Verbose output |
| `-run` | Run specific tests matching pattern |
| `-cover` | Enable coverage |
| `-coverprofile` | Write coverage profile to file |
| `-covermode` | Set coverage mode (set/count/atomic) |
| `-race` | Enable race detector |
| `-short` | Run short tests only |
| `-timeout` | Set test timeout (default 10m) |
| `-parallel` | Set parallel test count |
| `-count` | Run tests n times |
| `-failfast` | Stop on first test failure |

## Best Practices

1. **Use table-driven tests**: More maintainable and comprehensive
2. **Test behavior, not implementation**: Focus on what, not how
3. **Use t.Helper()**: Mark helper functions for better error reporting
4. **Prefer t.Error over t.Fatal**: Allow other tests to run
5. **Use t.Parallel() carefully**: Only for independent tests
6. **Mock external dependencies**: Use interfaces for testability
7. **Aim for meaningful coverage**: 80%+ for critical paths
8. **Run tests before committing**: `go test ./...` should pass
9. **Use -race regularly**: Catch concurrency issues early
10. **Keep tests fast**: Use `-short` for quick feedback

## Common Issues

### Tests Pass Individually but Fail Together
```bash
# Run tests sequentially
go test -p 1 ./...

# Check for shared state or race conditions
go test -race ./...
```

### Slow Tests
```bash
# Identify slow tests
go test -v ./... | grep -E "PASS|FAIL" | grep -E "[0-9]+\.[0-9]+s"

# Skip slow tests during development
go test -short ./...
```

### Flaky Tests
- Usually indicate race conditions or timing issues
- Run with `-race` to detect
- Use proper synchronization primitives
- Avoid `time.Sleep()` in tests

## Resources

- [Testing Package](https://pkg.go.dev/testing)
- [Table Driven Tests](https://go.dev/wiki/TableDrivenTests)
- [Go Test Comments](https://go.dev/wiki/CodeReviewComments#tests)
- [httptest Package](https://pkg.go.dev/net/http/httptest)
