---
name: golang
description: Go development best practices and patterns. USE WHEN writing Go code, designing Go projects, working with Go tools, testing, or integrating Go with Nix.
---

# Go Development Best Practices

## Purpose
Guide Go development following best practices and project conventions.

## Project Structure (for tools in ~/src/home)

### Standard Layout
```
tools/<tool-name>/
├── cmd/
│   └── <tool-name>/
│       └── main.go          # Entry point
├── internal/                # Private application code
│   ├── config/
│   ├── handlers/
│   └── models/
├── pkg/                     # Public library code (if needed)
├── go.mod                   # Module definition
├── go.sum                   # Dependency checksums
├── README.md
└── *_test.go               # Tests alongside code
```

### Integration with Nix
- Tools source: `/tools/<tool-name>/`
- Custom packages: `/pkgs/` (exposed via overlays)
- Testing: Run from tool directory with `go test ./...`
- Multi-arch: Build for x86_64-linux and aarch64-linux

## Best Practices

### Code Organization

#### Package Naming
- Use lowercase, single-word names
- No underscores or camelCase
- Example: `package config`, `package handler`

#### File Naming
- Use lowercase with underscores for multi-word names
- Example: `http_client.go`, `user_handler.go`
- Test files: `*_test.go`

### Writing Idiomatic Go

#### Error Handling
```go
// Good: Wrap errors with context
if err := doSomething(); err != nil {
    return fmt.Errorf("failed to do something: %w", err)
}

// Good: Check errors immediately
result, err := fetchData()
if err != nil {
    return err
}

// Bad: Ignoring errors
result, _ := fetchData()  // Don't do this
```

#### Use context.Context
```go
// Good: Accept context as first parameter
func ProcessRequest(ctx context.Context, req *Request) error {
    // Use ctx for cancellation, deadlines, values
    select {
    case <-ctx.Done():
        return ctx.Err()
    case result := <-process(req):
        return result
    }
}
```

#### Prefer Composition Over Inheritance
```go
// Good: Embed types for composition
type Server struct {
    logger *Logger
    cache  *Cache
}

// Good: Define interfaces
type DataStore interface {
    Get(key string) ([]byte, error)
    Set(key string, value []byte) error
}
```

#### Keep Functions Small and Focused
```go
// Good: Single responsibility
func validateEmail(email string) error {
    if !strings.Contains(email, "@") {
        return errors.New("invalid email format")
    }
    return nil
}

// Bad: Doing too much
func processUserAndSendEmail(user User) error {
    // Validation
    // Database operations
    // Email sending
    // Logging
    // ... (too many responsibilities)
}
```

### Naming Conventions

#### Variables
- Use camelCase for local variables: `userCount`, `httpClient`
- Use short names in small scopes: `i`, `err`, `ok`
- Use descriptive names in larger scopes: `userRepository`, `configManager`

#### Functions and Methods
- Use camelCase: `getUserByID`, `processRequest`
- Exported functions start with uppercase: `NewClient`, `ProcessData`
- Getters don't use "Get" prefix: `user.Name()`, not `user.GetName()`

#### Constants
- Use camelCase or uppercase with underscores
- Exported: `MaxRetries`, `DefaultTimeout`
- Private: `defaultBufferSize`, `maxConnections`

## Testing

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
    }

    for _, tt := range tests {
        t.Run(tt.name, func(t *testing.T) {
            err := validateEmail(tt.email)
            if (err != nil) != tt.wantErr {
                t.Errorf("validateEmail() error = %v, wantErr %v", err, tt.wantErr)
            }
        })
    }
}
```

### Test Organization
- Tests in same package: `package mypackage`
- Tests in separate package: `package mypackage_test` (black-box testing)
- Use subtests with `t.Run()` for clarity
- Mock external dependencies using interfaces

### Test Coverage
- Aim for meaningful coverage, not 100%
- Focus on critical paths and edge cases
- Use `go test -cover ./...` to check coverage
- Don't test trivial code

### Running Tests
```bash
# Run all tests
go test ./...

# Run with coverage
go test -cover ./...

# Run specific test
go test -run TestValidateEmail

# Verbose output
go test -v ./...

# Run tests before committing
go test ./...
```

## Common Patterns

### Configuration Management
```go
type Config struct {
    Port     int           `json:"port"`
    Timeout  time.Duration `json:"timeout"`
    LogLevel string        `json:"log_level"`
}

func LoadConfig(path string) (*Config, error) {
    data, err := os.ReadFile(path)
    if err != nil {
        return nil, fmt.Errorf("reading config: %w", err)
    }

    var cfg Config
    if err := json.Unmarshal(data, &cfg); err != nil {
        return nil, fmt.Errorf("parsing config: %w", err)
    }

    return &cfg, nil
}
```

### Graceful Shutdown
```go
func main() {
    ctx, cancel := context.WithCancel(context.Background())
    defer cancel()

    sigChan := make(chan os.Signal, 1)
    signal.Notify(sigChan, os.Interrupt, syscall.SIGTERM)

    go func() {
        <-sigChan
        log.Println("Shutting down...")
        cancel()
    }()

    if err := run(ctx); err != nil {
        log.Fatal(err)
    }
}
```

### HTTP Client with Timeout
```go
client := &http.Client{
    Timeout: 10 * time.Second,
    Transport: &http.Transport{
        MaxIdleConns:        100,
        MaxIdleConnsPerHost: 10,
        IdleConnTimeout:     90 * time.Second,
    },
}
```

## Nix Integration

### Building
```bash
# Build a Go tool package
nix build .#<package-name>

# Install to profile
nix profile install .#<package-name>

# Check what would be built
nix build .#<package-name> --dry-run
```

### Testing in Nix
```bash
# Enter dev shell
nix develop

# Run tests
cd tools/<tool-name>
go test ./...
```

### Package Definition
```nix
# In pkgs/<package-name>/default.nix
{ lib, buildGoModule }:

buildGoModule {
  pname = "tool-name";
  version = "1.0.0";

  src = ../../tools/tool-name;

  vendorHash = "sha256-...";

  meta = with lib; {
    description = "Tool description";
    license = licenses.mit;
  };
}
```

## Common Commands

### Formatting and Linting
```bash
# Format code
gofmt -w .

# Organize imports
goimports -w .

# Vet code
go vet ./...

# Run linter
golangci-lint run

# Fix linter issues automatically
golangci-lint run --fix
```

### Dependency Management
```bash
# Add dependency
go get github.com/pkg/errors

# Update dependencies
go get -u ./...

# Tidy modules
go mod tidy

# Verify dependencies
go mod verify

# Vendor dependencies
go mod vendor
```

### Build and Run
```bash
# Build
go build -o bin/myapp cmd/myapp/main.go

# Run
go run cmd/myapp/main.go

# Install
go install ./cmd/myapp
```

## Performance

### Benchmarking
```go
func BenchmarkFunction(b *testing.B) {
    for i := 0; i < b.N; i++ {
        doSomething()
    }
}
```

### Profiling
```bash
# CPU profiling
go test -cpuprofile cpu.prof -bench .

# Memory profiling
go test -memprofile mem.prof -bench .

# Analyze profile
go tool pprof cpu.prof
```

### Optimization Tips
- Use sync.Pool for frequently allocated objects
- Prefer slices over arrays when size varies
- Use buffered channels appropriately
- Avoid premature optimization - profile first!

## Common Pitfalls to Avoid

### ❌ Goroutine Leaks
```go
// Bad: Goroutine never exits
go func() {
    for {
        doWork()
    }
}()

// Good: Use context for cancellation
go func(ctx context.Context) {
    for {
        select {
        case <-ctx.Done():
            return
        default:
            doWork()
        }
    }
}(ctx)
```

### ❌ Race Conditions
```go
// Bad: Concurrent map access
var cache = make(map[string]string)

// Good: Use sync.Map or mutex
var cache sync.Map
```

### ❌ Defer in Loops
```go
// Bad: Defers accumulate
for _, file := range files {
    f, _ := os.Open(file)
    defer f.Close()  // Won't close until function exits!
}

// Good: Close immediately
for _, file := range files {
    func() {
        f, _ := os.Open(file)
        defer f.Close()
    }()
}
```

## Resources

- Effective Go: https://go.dev/doc/effective_go
- Go Code Review Comments: https://github.com/golang/go/wiki/CodeReviewComments
- Standard library documentation: https://pkg.go.dev/std

Remember: Write clear, simple, idiomatic Go code. When in doubt, check the standard library for examples.
